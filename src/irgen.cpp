#include "irgen.h"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Module.h>
#include "context.h"
#include "log.h"

namespace llvm {
  class LLVMContext;
}

namespace std {
  template<>
  struct hash<yang::internal::IrGenerator::metadata> {
    std::size_t operator()(yang::internal::IrGenerator::metadata v) const
    {
      return v;
    }
  };
}

namespace yang {
namespace internal {

IrGeneratorUnion::IrGeneratorUnion(llvm::Type* type)
  : type(type)
  , value(nullptr)
{
}

IrGeneratorUnion::IrGeneratorUnion(llvm::Value* value)
  : type(nullptr)
  , value(value)
{
}

IrGeneratorUnion::operator llvm::Type*() const
{
  return type;
}

IrGeneratorUnion::operator llvm::Value*() const
{
  return value;
}

IrGenerator::IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
                         symbol_frame& globals, const Context& context)
  : _context(context)
  , _module(module)
  , _engine(engine)
  , _builder(module.getContext())
  , _symbol_table(nullptr)
  , _metadata(nullptr)
{
  // Set up the global data type. Since each program is designed to support
  // multiple independent instances running simultaeneously, the idea is to
  // define a structure type with a field for each global variable. Each
  // function will take a pointer to the global data structure as an implicit
  // final parameter.
  std::vector<llvm::Type*> type_list;
  std::size_t number = 0;
  // The first element in the global data structure is always a pointer to the
  // Yang program instance with which the data is associated.
  type_list.push_back(void_ptr_type());
  ++number;
  // Since the global data structure may contain pointers to functions which
  // themselves take said implicit argument, we need to use an opaque named
  // struct create the potentially-recursive type.
  auto global_struct =
      llvm::StructType::create(module.getContext(), "global_data");
  _global_data = llvm::PointerType::get(global_struct, 0);
  for (const auto& pair : globals) {
    // Type-calculation must be kept up-to-date with new types, or globals of
    // that type will fail.
    llvm::Type* t = get_llvm_type(pair.second);
    type_list.push_back(t);
    _global_numbering[pair.first] = number++;
  }
  global_struct->setBody(type_list, false);

  // We need to generate a reverse trampoline function for each function in the
  // Context. User type member functions are present in the context function map
  // as well as free functions.
  for (const auto& pair : context.get_functions()) {
    create_reverse_trampoline_function(pair.second.type);
  }
}

IrGenerator::~IrGenerator()
{
  // Keep hash<metadata> in source file.
}

void IrGenerator::emit_global_functions()
{
  auto& b = _builder;

  // Register malloc and free. Malloc takes a pointer argument, since
  // we computer sizeof with pointer arithmetic. (Conveniently, it's
  // also a type of the correct bit-width.)
  auto malloc_ptr = get_native_function(
      "malloc", (yang::void_fp)&malloc,
      llvm::FunctionType::get(_global_data, _global_data, false));
  auto free_ptr = get_native_function(
      "free", (yang::void_fp)&free,
      llvm::FunctionType::get(void_type(), _global_data, false));

  // Create allocator function. It takes a pointer to the Yang program instance.
  auto alloc = llvm::Function::Create(
      llvm::FunctionType::get(_global_data, void_ptr_type(), false),
      llvm::Function::ExternalLinkage, "!global_alloc", &_module);
  alloc->arg_begin()->setName("instance");
  auto alloc_block = llvm::BasicBlock::Create(
      b.getContext(), "entry", alloc);
  b.SetInsertPoint(alloc_block);

  // Compute sizeof(_global_data) by indexing one past the null pointer.
  llvm::Value* size_of = b.CreateIntToPtr(
      constant_int(0), _global_data, "null");
  size_of = b.CreateGEP(
      size_of, constant_int(1), "sizeof");
  // Call malloc, set instance pointer, call each of the global initialisation
  // functions, and return the pointer.
  llvm::Value* v = b.CreateCall(malloc_ptr, size_of, "call");
  b.CreateStore(alloc->arg_begin(), global_ptr(v, 0));
  for (llvm::Function* f : _global_inits) {
    b.CreateCall(f, v);
  }
  b.CreateRet(v);

  // Create free function.
  auto free = llvm::Function::Create(
      llvm::FunctionType::get(void_type(), _global_data, false),
      llvm::Function::ExternalLinkage, "!global_free", &_module);
  auto free_block = llvm::BasicBlock::Create(
      b.getContext(), "entry", free);
  b.SetInsertPoint(free_block);
  auto it = free->arg_begin();
  it->setName("global");
  b.CreateCall(free_ptr, it);
  b.CreateRetVoid();

  // Create accessor functions for each field of the global structure.
  for (const auto pair : _global_numbering) {
    llvm::Type* t = _global_data->
        getPointerElementType()->getStructElementType(pair.second);

    std::string name = "!global_get_" + pair.first;
    auto function_type = llvm::FunctionType::get(t, _global_data, false);
    auto getter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
    create_trampoline_function(function_type);

    auto getter_block = llvm::BasicBlock::Create(
        b.getContext(), "entry", getter);
    auto it = getter->arg_begin();
    it->setName("global");
    b.SetInsertPoint(getter_block);
    b.CreateRet(
        b.CreateLoad(global_ptr(it, pair.second), "load"));

    name = "!global_set_" + pair.first;
    std::vector<llvm::Type*> setter_args{_global_data, t};
    function_type = llvm::FunctionType::get(void_type(), setter_args, false);
    auto setter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
    create_trampoline_function(function_type);

    auto setter_block = llvm::BasicBlock::Create(
        b.getContext(), "entry", setter);
    it = setter->arg_begin();
    it->setName("global");
    auto jt = it;
    ++jt;
    jt->setName("value");
    b.SetInsertPoint(setter_block);
    b.CreateStore(jt, global_ptr(it, pair.second));
    b.CreateRetVoid();
  }
}

const IrGenerator::trampoline_map& IrGenerator::get_trampoline_map() const
{
  return _trampoline_map;
}

void IrGenerator::preorder(const Node& node)
{
  auto& b = _builder;

  switch (node.type) {
    case Node::GLOBAL:
    {
      _metadata.push();
      // GLOBAL init functions don't need external linkage, since they are
      // called automatically by the externally-visible global structure
      // allocation function.
      auto function = llvm::Function::Create(
          llvm::FunctionType::get(void_type(), _global_data, false),
          llvm::Function::InternalLinkage, "!global_init", &_module);
      _global_inits.push_back(function);
      auto block = llvm::BasicBlock::Create(b.getContext(), "entry", function);

      _metadata.add(GLOBAL_INIT_FUNCTION, function);
      b.SetInsertPoint(block);
      _symbol_table.push();

      // Store a special entry in the symbol table for the implicit global
      // structure pointer.
      auto it = function->arg_begin();
      it->setName("global");
      _metadata.add(GLOBAL_DATA_PTR, it);
      _metadata.add(FUNCTION, function);
      break;
    }

    case Node::FUNCTION:
      _metadata.push();
      _metadata.add(TYPE_EXPR_CONTEXT, nullptr);
      break;
    case Node::GLOBAL_ASSIGN:
    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
      // See static.cpp for details on the immediate left assign hack.
      if (node.children[1]->type == Node::FUNCTION) {
        _immediate_left_assign = node.children[0]->string_value;
      }
      break;

    case Node::BLOCK:
      _symbol_table.push();
      break;

    case Node::IF_STMT:
    {
      _metadata.push();
      _symbol_table.push();
      create_block(IF_THEN_BLOCK, "then");
      create_block(MERGE_BLOCK, "merge");
      if (node.children.size() > 2) {
        create_block(IF_ELSE_BLOCK, "else");
      }
      break;
    }

    case Node::FOR_STMT:
    {
      _metadata.push();
      _symbol_table.push();
      create_block(LOOP_COND_BLOCK, "cond");
      create_block(LOOP_BODY_BLOCK, "loop");
      auto after_block = create_block(LOOP_AFTER_BLOCK, "after");
      auto merge_block = create_block(MERGE_BLOCK, "merge");
      _metadata.add(LOOP_BREAK_LABEL, merge_block);
      _metadata.add(LOOP_CONTINUE_LABEL, after_block);
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      _metadata.push();
      _symbol_table.push();
      auto loop_block =create_block(LOOP_BODY_BLOCK, "loop");
      auto cond_block = create_block(LOOP_COND_BLOCK, "cond");
      auto merge_block = create_block(MERGE_BLOCK, "merge");
      _metadata.add(LOOP_BREAK_LABEL, merge_block);
      _metadata.add(LOOP_CONTINUE_LABEL, cond_block);

      b.CreateBr(loop_block);
      b.SetInsertPoint(loop_block);
      break;
    }

    default: {}
  }
}

void IrGenerator::infix(const Node& node, const result_list& results)
{
  auto& b = _builder;

  switch (node.type) {
    case Node::FUNCTION:
    {
      _metadata.pop();
      create_function(node, function_type_from_generic(results[0].type));
      break;
    }

    case Node::IF_STMT:
    {
      auto then_block = (llvm::BasicBlock*)_metadata[IF_THEN_BLOCK];
      auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];

      if (results.size() == 1) {
        bool has_else = node.children.size() > 2;
        b.CreateCondBr(i2b(results[0]), then_block,
                       has_else ? else_block : merge_block);
        b.SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        b.CreateBr(merge_block);
        b.SetInsertPoint(else_block);
      }
      break;
    }

    case Node::FOR_STMT:
    {
      auto cond_block = (llvm::BasicBlock*)_metadata[LOOP_COND_BLOCK];
      auto loop_block = (llvm::BasicBlock*)_metadata[LOOP_BODY_BLOCK];
      auto after_block = (llvm::BasicBlock*)_metadata[LOOP_AFTER_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];

      if (results.size() == 1) {
        b.CreateBr(cond_block);
        b.SetInsertPoint(cond_block);
      }
      if (results.size() == 2) {
        b.CreateCondBr(i2b(results[1]), loop_block, merge_block);
        b.SetInsertPoint(after_block);
      }
      if (results.size() == 3) {
        b.CreateBr(cond_block);
        b.SetInsertPoint(loop_block);
      }
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      auto cond_block = (llvm::BasicBlock*)_metadata[LOOP_COND_BLOCK];
      b.CreateBr(cond_block);
      b.SetInsertPoint(cond_block);
      break;
    }

    case Node::TERNARY:
    {
      // Vectorised ternary can't short-circuit.
      if (results[0].value->getType()->isVectorTy()) {
        break;
      }

      if (results.size() == 1) {
        _metadata.push();

        // Blocks and branching are necessary (as opposed to a select
        // instruction) to short-circuit and avoiding evaluating the other path.
        auto then_block = create_block(IF_THEN_BLOCK, "then");
        auto else_block = create_block(IF_ELSE_BLOCK, "else");
        create_block(MERGE_BLOCK, "merge");

        b.CreateCondBr(i2b(results[0]), then_block, else_block);
        b.SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
        auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
        b.CreateBr(merge_block);
        b.SetInsertPoint(else_block);
      }
      break;
    }

    // Logical OR and AND short-circuit. This interacts in a subtle way with
    // vectorisation: we can short-circuit only when the left-hand operand is a
    // primitive. (We could also check and short-circuit if the left-hand
    // operand is an entirely zero or entirely non-zero vector. We currently
    // don't; it seems a bit daft.)
    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    {
      if (results[0].value->getType()->isVectorTy()) {
        break;
      }
      _metadata.push();
      _metadata.add(LOGICAL_OP_SOURCE_BLOCK, b.GetInsertPoint());
      auto rhs_block = create_block(LOGICAL_OP_RHS_BLOCK, "rhs");
      auto merge_block = create_block(MERGE_BLOCK, "merge");

      if (node.type == Node::LOGICAL_OR) {
        b.CreateCondBr(i2b(results[0]), merge_block, rhs_block);
      }
      else {
        b.CreateCondBr(i2b(results[0]), rhs_block, merge_block);
      }
      b.SetInsertPoint(rhs_block);
      break;
    }

    default: {}
  }
}

IrGeneratorUnion IrGenerator::visit(const Node& node,
                                    const result_list& results)
{
  auto& b = _builder;
  auto parent = b.GetInsertBlock() ? b.GetInsertBlock()->getParent() : nullptr;
  std::vector<llvm::Type*> types;
  for (const auto& v : results) {
    types.push_back(v.value ? v.value->getType() : nullptr);
  }

  auto binary_lambda = [&](llvm::Value* v, llvm::Value* u)
  {
    Node::node_type type =
        node.type == Node::FOLD_LOGICAL_OR ? Node::LOGICAL_OR :
        node.type == Node::FOLD_LOGICAL_AND ? Node::LOGICAL_AND :
        node.type == Node::FOLD_BITWISE_OR ? Node::BITWISE_OR :
        node.type == Node::FOLD_BITWISE_AND ? Node::BITWISE_AND :
        node.type == Node::FOLD_BITWISE_XOR ? Node::BITWISE_XOR :
        node.type == Node::FOLD_BITWISE_LSHIFT ? Node::BITWISE_LSHIFT :
        node.type == Node::FOLD_BITWISE_RSHIFT ? Node::BITWISE_RSHIFT :
        node.type == Node::FOLD_POW ? Node::POW :
        node.type == Node::FOLD_MOD ? Node::MOD :
        node.type == Node::FOLD_ADD ? Node::ADD :
        node.type == Node::FOLD_SUB ? Node::SUB :
        node.type == Node::FOLD_MUL ? Node::MUL :
        node.type == Node::FOLD_DIV ? Node::DIV :
        node.type == Node::FOLD_EQ ? Node::EQ :
        node.type == Node::FOLD_NE ? Node::NE :
        node.type == Node::FOLD_GE ? Node::GE :
        node.type == Node::FOLD_LE ? Node::LE :
        node.type == Node::FOLD_GT ? Node::GT :
        node.type == Node::FOLD_LT ? Node::LT :
        node.type;

    if (v->getType()->isIntOrIntVectorTy()) {
      return type == Node::LOGICAL_OR ? b.CreateOr(v, u, "lor") :
             type == Node::LOGICAL_AND ? b.CreateAnd(v, u, "land") :
             type == Node::BITWISE_OR ? b.CreateOr(v, u, "or") :
             type == Node::BITWISE_AND ? b.CreateAnd(v, u, "and") :
             type == Node::BITWISE_XOR ? b.CreateXor(v, u, "xor") :
             type == Node::BITWISE_LSHIFT ? b.CreateShl(v, u, "lsh") :
             type == Node::BITWISE_RSHIFT ? b.CreateAShr(v, u, "rsh") :
             type == Node::POW ? pow(v, u) :
             type == Node::MOD ? mod(v, u) :
             type == Node::ADD ? b.CreateAdd(v, u, "add") :
             type == Node::SUB ? b.CreateSub(v, u, "sub") :
             type == Node::MUL ? b.CreateMul(v, u, "mul") :
             type == Node::DIV ? div(v, u) :
             type == Node::EQ ? b.CreateICmpEQ(v, u, "eq") :
             type == Node::NE ? b.CreateICmpNE(v, u, "ne") :
             type == Node::GE ? b.CreateICmpSGE(v, u, "ge") :
             type == Node::LE ? b.CreateICmpSLE(v, u, "le") :
             type == Node::GT ? b.CreateICmpSGT(v, u, "gt") :
             type == Node::LT ? b.CreateICmpSLT(v, u, "lt") :
             nullptr;
    }
    return type == Node::POW ? pow(v, u) :
           type == Node::MOD ? mod(v, u) :
           type == Node::ADD ? b.CreateFAdd(v, u, "fadd") :
           type == Node::SUB ? b.CreateFSub(v, u, "fsub") :
           type == Node::MUL ? b.CreateFMul(v, u, "fmul") :
           type == Node::DIV ? div(v, u) :
           type == Node::EQ ? b.CreateFCmpOEQ(v, u, "feq") :
           type == Node::NE ? b.CreateFCmpONE(v, u, "fne") :
           type == Node::GE ? b.CreateFCmpOGE(v, u, "fge") :
           type == Node::LE ? b.CreateFCmpOLE(v, u, "fle") :
           type == Node::GT ? b.CreateFCmpOGT(v, u, "fgt") :
           type == Node::LT ? b.CreateFCmpOLT(v, u, "flt") :
           nullptr;
  };

  switch (node.type) {
    case Node::TYPE_VOID:
      return void_type();
    case Node::TYPE_INT:
      return node.int_value > 1 ?
          vector_type(int_type(), node.int_value) : int_type();
    case Node::TYPE_FLOAT:
      return node.int_value > 1 ?
          vector_type(float_type(), node.int_value) : float_type();
    case Node::TYPE_FUNCTION:
    {
      type_function:
      std::vector<llvm::Type*> args;
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i]);
      }
      args.push_back(_global_data);
      return generic_function_type(results[0], args);
    }

    case Node::GLOBAL:
      b.CreateRetVoid();
      _symbol_table.pop();
      _metadata.pop();
      return results[0];
    case Node::GLOBAL_ASSIGN:
    {
      auto function = (llvm::Function*)parent;
      auto function_type =
          (llvm::FunctionType*)function->getType()->getPointerElementType();
      create_trampoline_function(function_type);
      // Top-level functions Nodes have their int_value set to 1 when defined
      // using the `export` keyword.
      if (node.int_value) {
        function->setLinkage(llvm::Function::ExternalLinkage);
      }
      function->setName(node.children[0]->string_value);
      _symbol_table.add(node.children[0]->string_value, results[1]);
      return results[1];
    }
    case Node::FUNCTION:
    {
      auto function_type =
          (llvm::FunctionType*)parent->getType()->getElementType();
      if (function_type->getReturnType()->isVoidTy()) {
        b.CreateRetVoid();
      }
      else {
        // In a function that passes the static check, control never reaches
        // this point; but the block must have a terminator.
        b.CreateBr(_builder.GetInsertBlock());
      }
      _symbol_table.pop();
      _symbol_table.pop();
      _metadata.pop();
      if (!_metadata.has(FUNCTION)) {
        return parent;
      }
      // If this was a nested function, set the insert point back to the last
      // block in the enclosing function and make the proper expression value.
      auto function = (llvm::Function*)_metadata[FUNCTION];
      b.SetInsertPoint(&*function->getBasicBlockList().rbegin());
      return generic_function_value(parent);
    }
    case Node::NAMED_EXPRESSION:
      return results[0];
      break;

    case Node::BLOCK:
    {
      auto after_block =
          llvm::BasicBlock::Create(b.getContext(), "after", parent);
      b.CreateBr(after_block);
      b.SetInsertPoint(after_block);
      _symbol_table.pop();
      return parent;
    }
    case Node::EMPTY_STMT:
      return constant_int(0);
    case Node::EXPR_STMT:
      return results[0];
    case Node::RETURN_STMT:
    {
      auto dead_block =
          llvm::BasicBlock::Create(b.getContext(), "dead", parent);
      llvm::Value* v = b.CreateRet(results[0]);
      b.SetInsertPoint(dead_block);
      return v;
    }
    case Node::IF_STMT:
    {
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();
      b.CreateBr(merge_block);
      b.SetInsertPoint(merge_block);
      return results[0];
    }
    case Node::FOR_STMT:
    {
      auto after_block = (llvm::BasicBlock*)_metadata[LOOP_AFTER_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      b.CreateBr(after_block);
      b.SetInsertPoint(merge_block);
      return results[0];
    }
    case Node::DO_WHILE_STMT:
    {
      auto loop_block = (llvm::BasicBlock*)_metadata[LOOP_BODY_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      b.CreateCondBr(i2b(results[1]), loop_block, merge_block);
      b.SetInsertPoint(merge_block);
      return results[0];
    }
    case Node::BREAK_STMT:
    case Node::CONTINUE_STMT:
    {
      auto dead_block =
          llvm::BasicBlock::Create(b.getContext(), "dead", parent);
      llvm::Value* v = b.CreateBr((llvm::BasicBlock*)_metadata[
          node.type == Node::BREAK_STMT ? LOOP_BREAK_LABEL :
                                          LOOP_CONTINUE_LABEL]);
      b.SetInsertPoint(dead_block);
      return v;
    }

    case Node::SCOPE_RESOLUTION:
    {
      // Scope resolution always refers to a context function.
      std::string s = node.user_type_name + "::" + node.string_value;
      auto it = _context.get_functions().find(s);
      return generic_function_value(it->second);
    }
    case Node::MEMBER_SELECTION:
      // Just return the object directly; the call site has special logic to
      // deal with member functions.
      return results[0];

    case Node::IDENTIFIER:
    {
      // In type-context, we just want to return a user type.
      if (_metadata.has(TYPE_EXPR_CONTEXT)) {
        return void_ptr_type();
      }

      // Load the local variable, if it's there.
      if (_symbol_table.has(node.string_value) &&
          _symbol_table.index(node.string_value)) {
        return b.CreateLoad(_symbol_table[node.string_value], "load");
      }
      // Otherwise it's a top-level function, global, or context function. We
      // must be careful to only return globals/functions *after* they have been
      // defined, otherwise it might be a reference to a context function of the
      // same name.
      if (_symbol_table.has(node.string_value)) {
        // If the symbol table entry is non-null it's a function (constant
        // global), just get the value.
        if (_symbol_table.get(node.string_value, 0)) {
          return generic_function_value(
              _symbol_table.get(node.string_value, 0));
        }
        // Otherwise it's a global, so look up in the global structure.
        return b.CreateLoad(global_ptr(node.string_value), "load");
      }

      // It's possible that nothing matches, when this is the identifier on the
      // left of a variable declaration.
      auto it = _context.get_functions().find(node.string_value);
      if (it == _context.get_functions().end()) {
        return (llvm::Value*)nullptr;
      }

      // It must be a context function.
      return generic_function_value(it->second);
    }

    case Node::INT_LITERAL:
      return constant_int(node.int_value);
    case Node::FLOAT_LITERAL:
      return constant_float(node.float_value);

    case Node::TERNARY:
    {
      if (results[0].value->getType()->isVectorTy()) {
        // Vectorised ternary. Short-circuiting isn't possible.
        return b.CreateSelect(i2b(results[0]), results[1], results[2]);
      }
      auto then_block = (llvm::BasicBlock*)_metadata[IF_THEN_BLOCK];
      auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();
      b.CreateBr(merge_block);
      b.SetInsertPoint(merge_block);
      auto phi = b.CreatePHI(types[1], 2, "tern");
      phi->addIncoming(results[1], then_block);
      phi->addIncoming(results[2], else_block);
      return phi;
    }
    case Node::CALL:
    {
      if (_metadata.has(TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }

      // Special odd logic for member-function calling.
      if (node.children[0]->type == Node::MEMBER_SELECTION) {
        std::string s = node.children[0]->user_type_name +
            "::" + node.children[0]->string_value;

        std::vector<llvm::Value*> args;
        args.push_back(results[0]);
        for (std::size_t i = 1; i < results.size(); ++i) {
          args.push_back(results[i]);
        }
        args.push_back(_metadata[GLOBAL_DATA_PTR]);
        auto it = _context.get_functions().find(s);
        args.push_back(constant_ptr(it->second.ptr.get()));
        return b.CreateCall(
            _reverse_trampoline_map[it->second.type.erase_user_types()], args);
      }

      std::vector<llvm::Value*> args;
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i]);
      }
      args.push_back(_metadata[GLOBAL_DATA_PTR]);

      // Construct the function type which includes an extra target type at
      // the end. This is kind of weird. We could make every function have an
      // unused target parameter at the end to avoid this weird casting and
      // switching. Not sure whether that's a better idea.
      auto f_type = function_type_from_generic(types[0]);
      std::vector<llvm::Type*> ft_args;
      for (auto it = f_type->param_begin(); it != f_type->param_end(); ++it) {
        ft_args.push_back(*it);
      }
      ft_args.push_back(void_ptr_type());
      auto ft_type = llvm::PointerType::get(
          llvm::FunctionType::get(f_type->getReturnType(), ft_args, false), 0);

      // Extract pointers from the struct.
      llvm::Value* fptr = b.CreateExtractValue(results[0], 0, "fptr");
      llvm::Value* tptr = b.CreateExtractValue(results[0], 2, "tptr");

      // Switch on the presence of a trampoline function pointer.
      auto parent = b.GetInsertBlock()->getParent();
      auto cpp_block =
          llvm::BasicBlock::Create(b.getContext(), "cpp", parent);
      auto yang_block =
          llvm::BasicBlock::Create(b.getContext(), "yang", parent);
      auto merge_block =
          llvm::BasicBlock::Create(b.getContext(), "merge", parent);

      llvm::Value* cmp = b.CreateICmpNE(
          tptr,
          llvm::ConstantPointerNull::get((llvm::PointerType*)void_ptr_type()));
      b.CreateCondBr(cmp, cpp_block, yang_block);

      b.SetInsertPoint(yang_block);
      llvm::Value* yang_val = b.CreateCall(fptr, args);
      b.CreateBr(merge_block);

      args.push_back(tptr);
      b.SetInsertPoint(cpp_block);
      llvm::Value* cpp_val = b.CreateCall(
          b.CreateBitCast(fptr, ft_type, "cast"), args);
      b.CreateBr(merge_block);

      b.SetInsertPoint(merge_block);
      if (!f_type->getReturnType()->isVoidTy()) {
        auto phi = b.CreatePHI(f_type->getReturnType(), 2, "call");
        phi->addIncoming(cpp_val, cpp_block);
        phi->addIncoming(yang_val, yang_block);
        return phi;
      }
      return (llvm::Value*)nullptr;
    }

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    {
      if (types[0]->isVectorTy()) {
        // Short-circuiting isn't possible.
        return b2i(binary(i2b(results[0]), i2b(results[1]), binary_lambda));
      }
      auto source_block = (llvm::BasicBlock*)_metadata[LOGICAL_OP_SOURCE_BLOCK];
      auto rhs_block = (llvm::BasicBlock*)_metadata[LOGICAL_OP_RHS_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();

      auto rhs = b2i(i2b(results[1]));
      b.CreateBr(merge_block);
      b.SetInsertPoint(merge_block);
      llvm::Constant* constant =
          constant_int(node.type == Node::LOGICAL_OR ? 1 : 0);
      llvm::Type* type = int_type();
      if (types[1]->isVectorTy()) {
        std::size_t n = types[1]->getVectorNumElements();
        constant = constant_vector(constant, n);
        type = vector_type(type, n);
      }

      auto phi = b.CreatePHI(type, 2, "cut");
      phi->addIncoming(constant, source_block);
      phi->addIncoming(rhs, rhs_block);
      return phi;
    }

    // Most binary operators map directly to (vectorisations of) LLVM IR
    // instructions.
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
      return binary(results[0], results[1], binary_lambda);

    case Node::POW:
    case Node::MOD:
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
      return binary(results[0], results[1], binary_lambda);
    case Node::EQ:
    case Node::NE:
    case Node::GE:
    case Node::LE:
    case Node::GT:
    case Node::LT:
      return b2i(binary(results[0], results[1], binary_lambda));

    case Node::FOLD_LOGICAL_OR:
    case Node::FOLD_LOGICAL_AND:
      return b2i(fold(results[0], binary_lambda, true));

    case Node::FOLD_BITWISE_OR:
    case Node::FOLD_BITWISE_AND:
    case Node::FOLD_BITWISE_XOR:
    case Node::FOLD_BITWISE_LSHIFT:
    case Node::FOLD_BITWISE_RSHIFT:
      return fold(results[0], binary_lambda);

    case Node::FOLD_POW:
      // POW is the only right-associative fold operator.
      return fold(results[0], binary_lambda, false, false, true);
    case Node::FOLD_MOD:
    case Node::FOLD_ADD:
    case Node::FOLD_SUB:
    case Node::FOLD_MUL:
    case Node::FOLD_DIV:
      return fold(results[0], binary_lambda);

    case Node::FOLD_EQ:
    case Node::FOLD_NE:
    case Node::FOLD_GE:
    case Node::FOLD_LE:
    case Node::FOLD_GT:
    case Node::FOLD_LT:
      return b2i(fold(results[0], binary_lambda, false, true));

    case Node::LOGICAL_NEGATION:
    {
      llvm::Constant* cmp = constant_int(0);
      if (types[0]->isVectorTy()) {
        cmp = constant_vector(cmp, types[0]->getVectorNumElements());
      }
      return b2i(b.CreateICmpEQ(results[0], cmp, "lneg"));
    }
    case Node::BITWISE_NEGATION:
    {
      llvm::Constant* cmp = constant_int(0u - 1);
      if (types[0]->isVectorTy()) {
        cmp = constant_vector(cmp, types[0]->getVectorNumElements());
      }
      return b.CreateXor(results[0], cmp, "neg");
    }
    case Node::ARITHMETIC_NEGATION:
    {
      llvm::Constant* cmp = types[0]->isIntOrIntVectorTy() ?
          constant_int(0) : constant_float(0);
      if (types[0]->isVectorTy()) {
        cmp = constant_vector(cmp, types[0]->getVectorNumElements());
      }
      return types[0]->isIntOrIntVectorTy() ?
          b.CreateSub(cmp, results[0], "sub") :
          b.CreateFSub(cmp, results[0], "fsub");
    }

    case Node::ASSIGN:
    {
      // See Node::IDENTIFIER.
      const std::string& s = node.children[0]->string_value;
      if (_symbol_table[s]) {
        b.CreateStore(results[1], _symbol_table[s]);
      }
      // We can't store values for globals in the symbol table since the lookup
      // depends on the current function's global data argument.
      else {
        b.CreateStore(results[1], global_ptr(s));
      }
      return results[1];
    }

    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
    {
      const std::string& s = node.children[0]->string_value;
      // In a global block, rather than allocating anything we simply store into
      // the prepared fields of the global structure. Also enter the symbol now
      // with a null value so we can distinguish it from top-level functions.
      if (_metadata.has(GLOBAL_INIT_FUNCTION)) {
        _symbol_table.add(s, 0, nullptr);
        b.CreateStore(results[1], global_ptr(s));
        return results[1];
      }

      // Optimisation passes such as mem2reg work much better when memory
      // locations are declared in the entry block (so they are guaranteed to
      // execute once).
      auto& entry_block =
          ((llvm::Function*)b.GetInsertPoint()->getParent())->getEntryBlock();
      llvm::IRBuilder<> entry(&entry_block, entry_block.begin());
      llvm::Value* v = entry.CreateAlloca(types[1], nullptr, s);
      b.CreateStore(results[1], v);
      _symbol_table.add(s, v);
      return results[1];
    }

    case Node::INT_CAST:
      return w2i(results[0]);
    case Node::FLOAT_CAST:
      return i2w(results[0]);

    case Node::VECTOR_CONSTRUCT:
    {
      llvm::Value* v = constant_vector(
          types[0]->isIntegerTy() ?
              constant_int(0) : constant_float(0), results.size());
      for (std::size_t i = 0; i < results.size(); ++i) {
        v = b.CreateInsertElement(v, results[i], constant_int(i), "vec");
      }
      return v;
    }
    case Node::VECTOR_INDEX:
    {
      // Indexing out-of-bounds produces constant zero.
      llvm::Constant* zero = types[0]->isIntOrIntVectorTy() ?
          constant_int(0) : constant_float(0);
      auto ge = b.CreateICmpSGE(results[1], constant_int(0), "idx");
      auto lt = b.CreateICmpSLT(
          results[1], constant_int(types[0]->getVectorNumElements()), "idx");

      auto in = b.CreateAnd(ge, lt, "idx");
      return b.CreateSelect(
          in, b.CreateExtractElement(results[0], results[1], "idx"),
          zero, "idx");
    }

    default:
      return (llvm::Value*)nullptr;
  }
}

llvm::Function* IrGenerator::get_native_function(
    const std::string& name, yang::void_fp native_fp,
    llvm::FunctionType* type) const
{
  // We use special !-prefixed names for native functions so that they can't
  // be confused with regular user-defined functions (e.g. "malloc" is not
  // reserved).
  llvm::Function* llvm_function = llvm::Function::Create(
      type, llvm::Function::ExternalLinkage, "!" + name, &_module);
  // We need to explicitly link the LLVM function to the native function.
  // More (technically) undefined behaviour here.
  _engine.addGlobalMapping(llvm_function, (void*)(std::intptr_t)native_fp);
  return llvm_function;
}

void IrGenerator::create_function(
    const Node& node, llvm::FunctionType* function_type)
{
  auto& b = _builder;
  _metadata.push();

  // Linkage will be set later if necessary.
  auto function = llvm::Function::Create(
      function_type, llvm::Function::InternalLinkage,
      "anonymous", &_module);

  auto block = llvm::BasicBlock::Create(b.getContext(), "entry", function);
  b.SetInsertPoint(block);

  _symbol_table.push();
  // Recursive lookup handled similarly to arguments below.
  if (_immediate_left_assign.length()) {
    llvm::Value* v = b.CreateAlloca(
        llvm::PointerType::get(function_type, 0), nullptr,
        _immediate_left_assign);
    b.CreateStore(function, v);
    _symbol_table.add(_immediate_left_assign, v);
    _immediate_left_assign.clear();
  }
  _symbol_table.push();

  // Set up the arguments.
  std::size_t arg_num = 0;
  for (auto it = function->arg_begin(); it != --function->arg_end(); ++it) {
    const std::string& name =
        node.children[0]->children[1 + arg_num]->string_value;

    // Rather than reference argument values directly, we create an alloca
    // and store the argument in there. This simplifies things, since we
    // can emit the same IR code when referencing local variables or
    // function arguments.
    llvm::Value* alloc = b.CreateAlloca(
        function_type->getParamType(arg_num), nullptr, name);
    b.CreateStore(it, alloc);
    _symbol_table.add(name, alloc);
    ++arg_num;
  }
  // The code for Node::TYPE_FUNCTION in visit() ensures it takes a global
  // data structure pointer.
  auto global = --function->arg_end();
  global->setName("global");
  _metadata.add(GLOBAL_DATA_PTR, global);
  _metadata.add(FUNCTION, function);
}

llvm::Function* IrGenerator::create_trampoline_function(
    llvm::FunctionType* function_type)
{
  // Trampoline functions must be generated for every function type that might
  // be called externally or referenced by valid Function objects; reverse
  // trampolines must be generated for every function type which might be a
  // C++ function. This means:
  //
  // - trampolines for types of top-level functions;
  // - trampolines for types of generated global accessor functions;
  // - reverse trampolines for types of context functions;
  // - if a function type has some generated trampoline, its return function
  //   type must have the same kind of trampoline generated;
  // - if a function type has some generated trampoline, each argument function
  //   type must have the opposite kind of trampoline generated.
  // - the return type of any function type whose return type is also a function
  //   type, and for which a trampoline has been generated (transitively). This
  //   includes types of globals that have function type.
  //
  // Careful! User types have been erased by this point. Clients must erase
  // user types before looking up trampoline functions.
  //
  // It might be possible to further erase function pointers to minimise the
  // number of functions needed.
  yang::Type yang_type = get_yang_type(function_type);
  auto it = _trampoline_map.find(yang_type);
  if (it != _trampoline_map.end()) {
    return it->second;
  }

  auto& b = _builder;
  // LLVM bytecode calling convention has some significant drawbacks that make
  // it unsuitable for direct interop with native code. Most importantly,
  // calling convention for vectors in undefined; vectors can't be passed
  // directly at all. More subtly, calling convention for value structures
  // may be target-dependent.
  //
  // It's possible this implementation is overly conservative, but it's
  // definitely going to work. Essentially, we unpack vectors into individual
  // values, and convert return values to pointer arguments; the trampoline
  // takes the actual function to be called as the final argument.
  auto return_type = function_type->getReturnType();
  // Handle the transitive closure.
  if (get_yang_type(return_type).is_function()) {
    create_trampoline_function(function_type_from_generic(return_type));
  }
  for (auto it = function_type->param_begin();
       it != function_type->param_end(); ++it) {
    yang::Type t = get_yang_type(*it);
    if (t.is_function()) {
      create_reverse_trampoline_function(t);
    }
  }
  auto ext_function_type = get_trampoline_type(function_type, false);

  // Generate the function code.
  auto function = llvm::Function::Create(
      ext_function_type, llvm::Function::ExternalLinkage,
      "!trampoline", &_module);
  auto block = llvm::BasicBlock::Create(b.getContext(), "entry", function);
  b.SetInsertPoint(block);

  std::vector<llvm::Value*> call_args;
  auto callee = --function->arg_end();
  callee->setName("target");

  // Translate trampoline arguments to an LLVM-internal argument list.
  auto jt = function->arg_begin();
  for (std::size_t i = 0;
       i < get_trampoline_num_return_args(return_type); ++i) {
    jt->setName("r" + std::to_string(i));
    ++jt;
  }
  std::size_t i = 0;
  for (auto it = function_type->param_begin();
       it != function_type->param_end(); ++it, ++i) {
    if ((*it)->isVectorTy()) {
      std::size_t size = (*it)->getVectorNumElements();
      llvm::Value* v = (*it)->isIntOrIntVectorTy() ?
          constant_vector(constant_int(0), size) :
          constant_vector(constant_float(0), size);

      for (std::size_t j = 0; j < size; ++j) {
        jt->setName("a" + std::to_string(i) + "_" + std::to_string(j));
        v = b.CreateInsertElement(v, jt, constant_int(j), "vec");
        ++jt;
      }
      call_args.push_back(v);
      continue;
    }
    if ((*it)->isStructTy()) {
      jt->setName("a" + std::to_string(i) + "_fptr");
      llvm::Value* fptr = jt++;
      jt->setName("a" + std::to_string(i) + "_eptr");
      llvm::Value* eptr = jt++;
      jt->setName("a" + std::to_string(i) + "_tptr");
      llvm::Value* tptr = jt++;

      call_args.push_back(generic_function_value(fptr, eptr, tptr));
      continue;
    }
    // Global data is last parameter.
    auto kt = it;
    jt->setName(
        ++kt == function_type->param_end() ?
            "global_data" : "a" + std::to_string(i));
    call_args.push_back(jt++);
  }

  // Do the call and translate the result back to native calling convention.
  llvm::Value* result = b.CreateCall(callee, call_args);
  if (return_type->isVectorTy()) {
    auto it = function->arg_begin();
    for (std::size_t i = 0; i < return_type->getVectorNumElements(); ++i) {
      llvm::Value* v = b.CreateExtractElement(result, constant_int(i), "vec");
      b.CreateStore(v, it++);
    }
  }
  else if (return_type->isStructTy()) {
    llvm::Value* fptr = b.CreateExtractValue(result, 0, "fptr");
    llvm::Value* eptr = b.CreateExtractValue(result, 1, "eptr");
    llvm::Value* tptr = b.CreateExtractValue(result, 2, "tptr");

    auto it = function->arg_begin();
    b.CreateStore(fptr, it++);
    b.CreateStore(eptr, it++);
    b.CreateStore(tptr, it++);
  }
  else if (!return_type->isVoidTy()) {
    b.CreateStore(result, function->arg_begin());
  }
  b.CreateRetVoid();
  _trampoline_map.emplace(yang_type, function);
  return function;
}

llvm::Function* IrGenerator::create_reverse_trampoline_function(
    const yang::Type& function_type)
{
  auto& b = _builder;
  auto it = _reverse_trampoline_map.find(function_type.erase_user_types());
  if (it != _reverse_trampoline_map.end()) {
    return it->second;
  }
  // Handle transitive closure.
  if (function_type.get_function_return_type().is_function()) {
    create_reverse_trampoline_function(
        function_type.get_function_return_type());
  }
  for (std::size_t i = 0; i < function_type.get_function_num_args(); ++i) {
    yang::Type t = function_type.get_function_arg_type(i);
    if (t.is_function()) {
      create_trampoline_function(function_type_from_generic(get_llvm_type(t)));
    }
  }

  // Construct the type of the trampoline function (with extra argument for
  // target pointer).
  auto yang_function_type =
      function_type_from_generic(get_llvm_type(function_type));
  std::vector<llvm::Type*> ft_args;
  for (auto it = yang_function_type->param_begin();
       it != yang_function_type->param_end(); ++it) {
    ft_args.push_back(*it);
  }
  ft_args.push_back(void_ptr_type());
  auto internal_type = llvm::FunctionType::get(
      yang_function_type->getReturnType(), ft_args, false);

  // Generate it.
  auto function = llvm::Function::Create(
      internal_type, llvm::Function::InternalLinkage,
      "!reverse_trampoline", &_module);
  auto block = llvm::BasicBlock::Create(b.getContext(), "entry", function);
  b.SetInsertPoint(block);

  llvm::Type* return_type = internal_type->getReturnType();
  std::size_t return_args = get_trampoline_num_return_args(return_type);

  std::vector<llvm::Value*> return_allocs;
  std::vector<llvm::Value*> args;
  // Handle return args.
  for (std::size_t i = 0; i < return_args; ++i) {
    llvm::Type* t =
        return_type->isVectorTy() ? return_type->getVectorElementType() :
        return_type->isStructTy() ?
            ((llvm::StructType*)return_type)->getElementType(i) : return_type;
    llvm::Value* v = b.CreateAlloca(t, nullptr, "r" + std::to_string(i));
    return_allocs.push_back(v);
    args.push_back(v);
  }

  std::size_t i = 0;
  for (auto it = function->arg_begin(); it != function->arg_end(); ++it, ++i) {
    auto jt = it;
    if (++jt == function->arg_end()) {
      // Target is the last argument.
      it->setName("target");
    }
    else if (++jt == function->arg_end()) {
      // Global data is second-last argument.
      it->setName("global_data");
    }
    else {
      it->setName("a" + std::to_string(i));
    }

    if (it->getType()->isVectorTy()) {
      for (std::size_t j = 0; j < it->getType()->getVectorNumElements(); ++j) {
        llvm::Value* v = b.CreateExtractElement(it, constant_int(j), "vec");
        args.push_back(v);
      }
      continue;
    }
    if (it->getType()->isStructTy()) {
      llvm::Value* fptr = b.CreateExtractValue(it, 0, "fptr");
      llvm::Value* eptr = b.CreateExtractValue(it, 1, "eptr");
      llvm::Value* tptr = b.CreateExtractValue(it, 2, "tptr");
      args.push_back(fptr);
      args.push_back(eptr);
      args.push_back(tptr);
      continue;
    }
    args.push_back(it);
  }

  // Trampolines on the C++ side have been populated by template instantiations.
  // We may be providing a null pointer here, if C++ never uses this type. The
  // great thing is: LLVM doesn't care until this code is JIT-compiled, and
  // by construction all compilations are triggered by a mechanism which causes
  // the correct instantiations.
  yang::void_fp external_trampoline_ptr =
      get_cpp_trampoline_lookup_map()[function_type.erase_user_types()];
  auto external_type = get_trampoline_type(internal_type, true);
  auto external_trampoline = get_native_function(
      "external_trampoline", external_trampoline_ptr, external_type);
  b.CreateCall(external_trampoline, args);

  if (return_type->isVoidTy()) {
    b.CreateRetVoid();
  }
  else if (return_type->isVectorTy()) {
    llvm::Value* v = constant_vector(
        return_type->isIntOrIntVectorTy() ? constant_int(0) : constant_float(0),
        return_args);
    for (std::size_t i = 0; i < return_args; ++i) {
      v = b.CreateInsertElement(
          v, b.CreateLoad(return_allocs[i], "load"), constant_int(i), "vec");
    }
    b.CreateRet(v);
  }
  else if (return_type->isStructTy()) {
    b.CreateRet(generic_function_value(
        b.CreateLoad(return_allocs[0], "fptr"),
        b.CreateLoad(return_allocs[1], "eptr"),
        b.CreateLoad(return_allocs[2], "tptr")));
  }
  else {
    b.CreateRet(b.CreateLoad(return_allocs[0], "ret"));
  }
  _reverse_trampoline_map.emplace(function_type.erase_user_types(), function);
  return function;
}

llvm::FunctionType* IrGenerator::get_trampoline_type(
    llvm::FunctionType* function_type, bool reverse) const
{
  std::vector<llvm::Type*> args;
  auto add_type = [&](llvm::Type* t, bool to_ptr)
  {
    if (t->isVectorTy()) {
      for (std::size_t i = 0; i < t->getVectorNumElements(); ++i) {
        auto elem = t->getVectorElementType();
        args.push_back(to_ptr ? llvm::PointerType::get(elem, 0) : elem);
      }
      return;
    }
    if (t->isStructTy()) {
      auto u = (llvm::StructType*)t;
      for (auto it = u->element_begin(); it != u->element_end(); ++it) {
        args.push_back(to_ptr ? llvm::PointerType::get(*it, 0) : *it);
      }
      return;
    }
    args.push_back(to_ptr ? llvm::PointerType::get(t, 0) : t);
  };

  auto return_type = function_type->getReturnType();
  if (!return_type->isVoidTy()) {
    add_type(return_type, true);
  }
  for (auto it = function_type->param_begin();
       it != function_type->param_end(); ++it) {
    add_type(*it, false);
  }

  if (!reverse) {
    // Argument is pointer to Yang code function.
    args.push_back(llvm::PointerType::get(function_type, 0));
  }
  return llvm::FunctionType::get(void_type(), args, false);
}

std::size_t IrGenerator::get_trampoline_num_return_args(
    llvm::Type* return_type) const
{
  return
      return_type->isVoidTy() ? 0 :
      return_type->isVectorTy() ? return_type->getVectorNumElements() :
      return_type->isStructTy() ? 3 : 1;
}

llvm::Type* IrGenerator::void_ptr_type() const
{
  // LLVM doesn't have a built-in void pointer type, so just use a pointer
  // to whatever.
  return llvm::PointerType::get(int_type(), 0);
}

llvm::Type* IrGenerator::void_type() const
{
  return llvm::Type::getVoidTy(_builder.getContext());
}

llvm::Type* IrGenerator::int_type() const
{
  return llvm::IntegerType::get(
      _builder.getContext(), 8 * sizeof(Node::int_value));
}

llvm::Type* IrGenerator::float_type() const
{
  return llvm::Type::getDoubleTy(_builder.getContext());
}

llvm::Type* IrGenerator::vector_type(llvm::Type* type, std::size_t n) const
{
  return llvm::VectorType::get(type, n);
}

llvm::Type* IrGenerator::generic_function_type(llvm::Type* type) const
{
  std::vector<llvm::Type*> types;
  // Yang function pointer or trampoline pointer.
  types.push_back(type);
  // Pointer to environment (global data structure or closure structure).
  types.push_back(void_ptr_type());
  // C++ native target pointer.
  types.push_back(void_ptr_type());

  return llvm::StructType::get(_builder.getContext(), types);
}

llvm::Type* IrGenerator::generic_function_type(
    llvm::Type* return_type, const std::vector<llvm::Type*>& arg_types) const
{
  return generic_function_type(llvm::PointerType::get(
      llvm::FunctionType::get(return_type, arg_types, false), 0));
}

llvm::FunctionType* IrGenerator::function_type_from_generic(
    llvm::Type* generic_function_type) const
{
  auto struct_type = (llvm::StructType*)generic_function_type;
  auto f_type = (*struct_type->element_begin())->getPointerElementType();
  return (llvm::FunctionType*)f_type;
}

llvm::Value* IrGenerator::generic_function_value(
    llvm::Value* function_ptr,
    llvm::Value* env_ptr, llvm::Value* target_ptr)
{
  auto type = (llvm::StructType*)generic_function_type(function_ptr->getType());

  std::vector<llvm::Constant*> values;
  values.push_back(llvm::ConstantPointerNull::get(
      (llvm::PointerType*)type->getElementType(0)));
  values.push_back(llvm::ConstantPointerNull::get(
      (llvm::PointerType*)void_ptr_type()));
  values.push_back(llvm::ConstantPointerNull::get(
      (llvm::PointerType*)void_ptr_type()));

  llvm::Value* v = llvm::ConstantStruct::get(type, values);
  v = _builder.CreateInsertValue(v, function_ptr, 0, "fptr");
  if (env_ptr) {
    v = _builder.CreateInsertValue(v, env_ptr, 1, "eptr");
  }
  if (target_ptr) {
    v = _builder.CreateInsertValue(v, target_ptr, 2, "tptr");
  }
  return v;
}

llvm::Value* IrGenerator::generic_function_value(
    const GenericNativeFunction& function)
{
  llvm::Value* f = _reverse_trampoline_map[function.type.erase_user_types()];

  // Chop off the last argument so that this behaves like a regular function
  // of the type.
  std::vector<llvm::Type*> args;
  auto ft = (llvm::FunctionType*)f->getType()->getPointerElementType();
  for (std::size_t i = 0; i < ft->getNumParams() - 1; ++i) {
    args.push_back(ft->getParamType(i));
  }

  auto corrected_ft = llvm::PointerType::get(
      llvm::FunctionType::get(ft->getReturnType(), args, false), 0);
  f = _builder.CreateBitCast(f, corrected_ft, "fun");
  return generic_function_value(f, nullptr, constant_ptr(function.ptr.get()));
}

llvm::Constant* IrGenerator::constant_int(yang::int_t value) const
{
  return llvm::ConstantInt::getSigned(int_type(), value);
}

llvm::Constant* IrGenerator::constant_float(yang::float_t value) const
{
  return llvm::ConstantFP::get(_builder.getContext(), llvm::APFloat(value));
}

llvm::Constant* IrGenerator::constant_vector(
    const std::vector<llvm::Constant*>& values) const
{
  return llvm::ConstantVector::get(values);
}

llvm::Value* IrGenerator::constant_ptr(void* ptr)
{
  // To construct a constant pointer, we need to do a bit of machine-dependent
  // stuff.
  llvm::Type* int_ptr =
      llvm::IntegerType::get(_builder.getContext(), 8 * sizeof(ptr));
  llvm::Constant* const_int =
      llvm::ConstantInt::get(int_ptr, (std::size_t)ptr);
  llvm::Value* const_ptr =
      llvm::ConstantExpr::getIntToPtr(const_int, void_ptr_type());
  return const_ptr;
}

llvm::Constant* IrGenerator::constant_vector(
    llvm::Constant* value, std::size_t n) const
{
  return llvm::ConstantVector::getSplat(n, value);
}

llvm::Value* IrGenerator::i2b(llvm::Value* v)
{
  llvm::Constant* cmp = constant_int(0);
  if (v->getType()->isVectorTy()) {
    cmp = constant_vector(cmp, v->getType()->getVectorNumElements());
  }
  return _builder.CreateICmpNE(v, cmp, "bool");
}

llvm::Value* IrGenerator::b2i(llvm::Value* v)
{
  llvm::Type* type = int_type();
  if (v->getType()->isVectorTy()) {
    type = vector_type(type, v->getType()->getVectorNumElements());
  }
  return _builder.CreateZExt(v, type, "int");
}

llvm::Value* IrGenerator::i2w(llvm::Value* v)
{
  llvm::Type* type = float_type();
  if (v->getType()->isVectorTy()) {
    type = vector_type(type, v->getType()->getVectorNumElements());
  }
  return _builder.CreateSIToFP(v, type, "wrld");
}

llvm::Value* IrGenerator::w2i(llvm::Value* v)
{
  auto& b = _builder;

  llvm::Type* back_type = float_type();
  llvm::Type* type = int_type();
  llvm::Constant* zero = constant_float(0);
  if (v->getType()->isVectorTy()) {
    std::size_t n = v->getType()->getVectorNumElements();
    back_type = vector_type(back_type, n);
    type = vector_type(type, n);
    zero = constant_vector(zero, n);
  }
  // Mathematical floor. Implements the algorithm:
  // return int(v) - (v < 0 && v != float(int(v)));
  auto cast = b.CreateFPToSI(v, type, "int");
  auto back = b.CreateSIToFP(cast, back_type, "int");

  auto a_check = b.CreateFCmpOLT(v, zero, "int");
  auto b_check = b.CreateFCmpONE(v, back, "int");
  return b.CreateSub(cast,
      b2i(b.CreateAnd(a_check, b_check, "int")), "int");
}

llvm::Value* IrGenerator::global_ptr(llvm::Value* ptr, std::size_t index)
{
  // The first index indexes the global data pointer itself, i.e. to obtain
  // the one and only global data structure at that memory location.
  std::vector<llvm::Value*> indexes{constant_int(0), constant_int(index)};
  llvm::Value* v = _builder.CreateGEP(ptr, indexes, "global");
  return v;
}

llvm::Value* IrGenerator::global_ptr(const std::string& name)
{
  return global_ptr(_metadata[GLOBAL_DATA_PTR], _global_numbering[name]);
}

llvm::Value* IrGenerator::pow(llvm::Value* v, llvm::Value* u)
{
  auto& b = _builder;
  llvm::Type* t = v->getType();

  std::vector<llvm::Type*> args{float_type(), float_type()};
  auto pow_ptr = get_native_function(
      "pow", (yang::void_fp)&::pow,
      llvm::FunctionType::get(float_type(), args, false));

  if (t->isIntOrIntVectorTy()) {
    v = i2w(v);
    u = i2w(u);
  }

  if (!t->isVectorTy()) {
    std::vector<llvm::Value*> args{v, u};
    llvm::Value* r = b.CreateCall(pow_ptr, args, "pow");
    return t->isIntOrIntVectorTy() ? w2i(r) : r;
  }

  llvm::Value* result =
      constant_vector(constant_float(0), t->getVectorNumElements());
  for (std::size_t i = 0; i < t->getVectorNumElements(); ++i) {
    llvm::Value* x = b.CreateExtractElement(v, constant_int(i), "pow");
    llvm::Value* y = b.CreateExtractElement(u, constant_int(i), "pow");
    std::vector<llvm::Value*> args{x, y};

    llvm::Value* call = b.CreateCall(pow_ptr, args, "pow");
    result = b.CreateInsertElement(result, call, constant_int(i), "pow");
  }
  return t->isIntOrIntVectorTy() ? w2i(result) : result;
}

llvm::Value* IrGenerator::mod(llvm::Value* v, llvm::Value* u)
{
  auto& b = _builder;
  if (!v->getType()->isIntOrIntVectorTy()) {
    return b.CreateFRem(v, u, "fmod");
  }

  // Implements the following algorithm:
  // return (v >= 0 ? v : v + (bool(|v| % |u|) + |v| / |u|) * |u|) % |u|;
  // There are simpler ways, but they are vulnerable to overflow errors.
  // k = |v| % |u| + |v| / |u| is the smallest postive integer such that
  // k * |u| >= |v|.
  auto v_check = b.CreateICmpSGE(v, constant_int(0), "mod");
  auto u_check = b.CreateICmpSGE(u, constant_int(0), "mod");
  auto v_abs = b.CreateSelect(
      v_check, v, b.CreateSub(constant_int(0), v, "mod"), "mod");
  auto u_abs = b.CreateSelect(
      u_check, u, b.CreateSub(constant_int(0), u, "mod"), "mod");

  auto k = b.CreateAdd(b2i(i2b(b.CreateSRem(v_abs, u_abs, "mod"))),
                       b.CreateSDiv(v_abs, u_abs, "mod"), "mod");
  auto lhs = b.CreateSelect(
      v_check, v,
      b.CreateAdd(v, b.CreateMul(k, u_abs, "mod"), "mod"), "mod");
  return b.CreateSRem(lhs, u_abs, "mod");
}

llvm::Value* IrGenerator::div(llvm::Value* v, llvm::Value* u)
{
  auto& b = _builder;
  if (!v->getType()->isIntOrIntVectorTy()) {
    return b.CreateFDiv(v, u, "fdiv");
  }

  // Implements the following algorithm:
  // bool sign = (v < 0) == (u < 0);
  // int t = (v < 0 ? -(1 + v) : v) / |u|;
  // return (sign ? t : -(1 + t)) + (u < 0);
  auto v_check = b.CreateICmpSLT(v, constant_int(0), "div");
  auto u_check = b.CreateICmpSLT(u, constant_int(0), "div");
  auto sign = b.CreateICmpEQ(v_check, u_check, "div");
  auto u_abs = b.CreateSelect(
      u_check, b.CreateSub(constant_int(0), u, "div"), u, "div");

  auto t = b.CreateSelect(
      v_check,
      b.CreateSub(constant_int(-1), v, "div"), v, "div");
  t = b.CreateSDiv(t, u_abs, "div");
  return b.CreateAdd(
      b.CreateSelect(sign, t, b.CreateSub(constant_int(-1), t, "div"), "div"),
      b2i(u_check), "div");
}

llvm::Value* IrGenerator::binary(
    llvm::Value* left, llvm::Value* right,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> op)
{
  llvm::Type* l_type = left->getType();
  llvm::Type* r_type = right->getType();

  // If both scalar or vector, sizes must be equal, and we can directly operate
  // on the values.
  if (l_type->isVectorTy() == r_type->isVectorTy()) {
    return op(left, right);
  }

  // Otherwise one is a scalar and one a vector (but having the same base type),
  // and we need to extend the scalar to match the size of the vector.
  bool is_left = l_type->isVectorTy();
  std::size_t size = is_left ?
      l_type->getVectorNumElements() : r_type->getVectorNumElements();

  llvm::Value* v = nullptr;
  if (l_type->isIntOrIntVectorTy()) {
    v = constant_vector(constant_int(0), size);
    // Can't insert booleans into a vector of int_type()!
    llvm::Type* i = is_left ? l_type->getVectorElementType() : l_type;
    if (i->getIntegerBitWidth() == 1) {
      v = i2b(v);
    }
  }
  else {
    v = constant_vector(constant_float(0), size);
  }

  for (std::size_t i = 0; i < size; ++i) {
    v = _builder.CreateInsertElement(
        v, is_left ? right : left, constant_int(i), "vec");
  }
  return is_left ? op(left, v) : op(v, right);
}

llvm::Value* IrGenerator::fold(
    llvm::Value* value,
    std::function<llvm::Value*(llvm::Value*, llvm::Value*)> op,
    bool to_bool, bool with_ands, bool right_assoc)
{
  // Convert each argument to boolean, if necessary.
  std::vector<llvm::Value*> elements;
  for (std::size_t i = 0; i < value->getType()->getVectorNumElements(); ++i) {
    llvm::Value* v =
        _builder.CreateExtractElement(value, constant_int(i), "idx");
    elements.push_back(to_bool ? i2b(v) : v);
  }

  // Usually, we just form the chain (((e0 op e1) op e2) ...).
  if (!with_ands) {
    if (right_assoc) {
      auto it = elements.rbegin();
      llvm::Value* v = *it++;
      for (; it != elements.rend(); ++it) {
        v = op(*it, v);
      }
      return v;
    }
    auto it = elements.begin();
    llvm::Value* v = *it++;
    for (; it != elements.end(); ++it) {
      v = op(*it, v);
    }
    return v;
  }

  // For comparisons that isn't very useful, so instead form the chain
  // (e0 op e1) && (e1 op e2) && ...
  std::vector<llvm::Value*> comparisons;
  for (std::size_t i = 1; i < elements.size(); ++i) {
    comparisons.push_back(op(elements[i - 1], elements[i]));
  }

  // Ignore right_assoc, since logical AND is associative.
  llvm::Value* v = comparisons[0];
  for (std::size_t i = 1; i < comparisons.size(); ++i) {
    v = _builder.CreateAnd(v, comparisons[i], "fold");
  }
  return v;
}

llvm::Type* IrGenerator::get_llvm_type(const yang::Type& t) const
{
  if (t.is_function()) {
    std::vector<llvm::Type*> args;
    for (std::size_t i = 0; i < t.get_function_num_args(); ++i) {
      args.push_back(get_llvm_type(t.get_function_arg_type(i)));
    }
    args.push_back(_global_data);

    return generic_function_type(
        get_llvm_type(t.get_function_return_type()), args);
  }
  if (t.is_int()) {
    return int_type();
  }
  if (t.is_float()) {
    return float_type();
  }
  if (t.is_int_vector()) {
    return vector_type(int_type(), t.get_vector_size());
  }
  if (t.is_float_vector()) {
    return vector_type(float_type(), t.get_vector_size());
  }
  if (t.is_user_type()) {
    return void_ptr_type();
  }
  return void_type();
}

yang::Type IrGenerator::get_yang_type(llvm::Type* t) const
{
  yang::Type r;
  if (t == void_ptr_type()) {
    // We can't reconstruct the full user type from the void pointer. This means
    // we treat all user-types as equivalent for the purposes of trampoline
    // function generation (which makes sense).
    r._base = yang::Type::USER_TYPE;
  }
  else if (t->isFunctionTy() || t->isStructTy()) {
    auto ft = t->isStructTy() ?
        function_type_from_generic(t) : (llvm::FunctionType*)t;
    r._base = yang::Type::FUNCTION;
    r._elements.push_back(get_yang_type(ft->getReturnType()));
    // Make sure to skip the global data pointer.
    for (std::size_t i = 0; i < ft->getFunctionNumParams() - 1; ++i) {
      r._elements.push_back(get_yang_type(ft->getFunctionParamType(i)));
    }
  }
  else if (t->isIntOrIntVectorTy()) {
    r._base = yang::Type::INT;
  }
  else if (t->isFPOrFPVectorTy()) {
    r._base = yang::Type::FLOAT;
  }
  r._count = t->isVectorTy() ? t->getVectorNumElements() : 1;
  return r;
}

llvm::BasicBlock* IrGenerator::create_block(
    metadata meta, const std::string& name)
{
  auto& b = _builder;
  auto parent = b.GetInsertBlock() ? b.GetInsertBlock()->getParent() : nullptr;
  auto block = llvm::BasicBlock::Create(b.getContext(), name, parent);
  _metadata.add(meta, block);
  return block;
}

// End namespace yang::internal.
}
}
