//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irgen.h"

#include <llvm/IR/Module.h>
#include <yang/context.h>

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

namespace {
void refcount_function(void* target, yang::int_t change)
{
  auto native = (yang::internal::NativeFunction<void>*)target;
  while (change > 0) {
    native->take_reference();
    --change;
  }
  while (change < 0) {
    native->release_reference();
    ++change;
  }
}
// End anonymous namespace.
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

// TODO: this whole file needs refactored, to use a wrapped Value class that
// knows about Yang types instead of LLVM types.
IrGenerator::IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
                         symbol_frame& globals, const Context& context)
  : IrCommon(module, engine)
  , _module(module)
  , _context(context)
  , _symbol_table(nullptr)
  , _metadata(nullptr)
  , _scope_to_function_map{{0, 0}}
  , _function_scope(0)
  , _refcount_function(get_native_function(
      "refcount_function", (void_fp)&refcount_function,
      llvm::FunctionType::get(
          void_type(),
          std::vector<llvm::Type*>{void_ptr_type(), int_type()},
          false)))
{
  // Set up the global data type. Since each program is designed to support
  // multiple independent instances running simultaeneously, the idea is to
  // define a structure type with a field for each global variable. Each
  // function will take a pointer to the global data structure as an implicit
  // final parameter.
  init_structure_type(_global_data, _global_numbering, globals, "global_data");

  // We need to generate a reverse trampoline function for each function in the
  // Context. User type member functions are present in the context function map
  // as well as free functions.
  for (const auto& pair : context.get_functions()) {
    get_reverse_trampoline_function(pair.second.type);
  }
}

IrGenerator::~IrGenerator()
{
  // Keep hash<metadata> in source file.
}

void IrGenerator::emit_global_functions()
{
  // Register free.
  auto free_ptr = get_native_function(
      "free", (yang::void_fp)&free,
      llvm::FunctionType::get(void_type(), _global_data, false));

  // Create allocator function. It takes a pointer to the Yang program instance.
  auto alloc = llvm::Function::Create(
      llvm::FunctionType::get(_global_data, void_ptr_type(), false),
      llvm::Function::ExternalLinkage, "!global_alloc", &_module);
  alloc->arg_begin()->setName("instance");
  auto alloc_block = llvm::BasicBlock::Create(
      b().getContext(), "entry", alloc);
  b().SetInsertPoint(alloc_block);

  llvm::Value* v = allocate_structure_value(_global_data, _global_numbering);
  memory_store(alloc->arg_begin(), structure_ptr(v, 0));
  for (llvm::Function* f : _global_inits) {
    b().CreateCall(f, v);
  }
  b().CreateRet(v);

  // Create free function.
  // TODO: this should also be able to call user-defined script destructors.
  auto free = llvm::Function::Create(
      llvm::FunctionType::get(void_type(), _global_data, false),
      llvm::Function::ExternalLinkage, "!global_free", &_module);
  auto free_block = llvm::BasicBlock::Create(
      b().getContext(), "entry", free);
  b().SetInsertPoint(free_block);
  auto it = free->arg_begin();
  it->setName("global");
  // Make sure to decrement the reference count on each global variable on
  // destruction.
  for (const auto pair : _global_numbering) {
    llvm::Value* old = memory_load(structure_ptr(it, pair.second));
    update_reference_count(old, -1);
  }
  b().CreateCall(free_ptr, it);
  b().CreateRetVoid();

  // Create accessor functions for each field of the global structure.
  for (const auto pair : _global_numbering) {
    llvm::Type* t = _global_data->
        getPointerElementType()->getStructElementType(pair.second);

    std::string name = "!global_get_" + pair.first;
    auto function_type = llvm::FunctionType::get(t, _global_data, false);
    auto getter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
    get_trampoline_function(get_yang_type(function_type));

    auto getter_block = llvm::BasicBlock::Create(
        b().getContext(), "entry", getter);
    auto it = getter->arg_begin();
    it->setName("global");
    b().SetInsertPoint(getter_block);
    // Loads out of the global data structure must be byte-aligned! I don't
    // entirely understand why, but leaving the default will segfault at random
    // sometimes for certain types (e.g. float vectors).
    b().CreateRet(memory_load(structure_ptr(it, pair.second)));

    name = "!global_set_" + pair.first;
    std::vector<llvm::Type*> setter_args{t, _global_data};
    function_type = llvm::FunctionType::get(void_type(), setter_args, false);
    auto setter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
    get_trampoline_function(get_yang_type(function_type));

    auto setter_block = llvm::BasicBlock::Create(
        b().getContext(), "entry", setter);
    it = setter->arg_begin();
    it->setName("value");
    auto jt = it;
    ++jt;
    jt->setName("global");
    b().SetInsertPoint(setter_block);
    memory_store(it, structure_ptr(jt, pair.second));
    b().CreateRetVoid();
  }
}

void IrGenerator::preorder(const Node& node)
{
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
      auto block =
          llvm::BasicBlock::Create(b().getContext(), "entry", function);

      // Store a special entry in the symbol table for the implicit global
      // structure pointer.
      auto it = function->arg_begin();
      it->setName("env");
      _metadata.add(ENVIRONMENT_PTR, it);
      _metadata.add(FUNCTION, function);
      _metadata.add(PARENT_BLOCK, b().GetInsertBlock());
      allocate_closure_struct(node.static_info.closed_environment, it);

      _metadata.add(GLOBAL_INIT_FUNCTION, function);
      b().SetInsertPoint(block);
      _symbol_table.push();
      _refcount_locals.emplace_back();
      _refcount_locals.back().emplace_back();
      break;
    }

    case Node::FUNCTION:
      _metadata.push();
      _metadata.add(TYPE_EXPR_CONTEXT, nullptr);
      _refcount_locals.emplace_back();
      _refcount_locals.back().emplace_back();
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
      _refcount_locals.back().emplace_back();
      break;

    case Node::IF_STMT:
    {
      _metadata.push();
      _symbol_table.push();
      _refcount_locals.back().emplace_back();
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
      _refcount_locals.back().emplace_back();
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
      _refcount_locals.back().emplace_back();
      auto loop_block =create_block(LOOP_BODY_BLOCK, "loop");
      auto cond_block = create_block(LOOP_COND_BLOCK, "cond");
      auto merge_block = create_block(MERGE_BLOCK, "merge");
      _metadata.add(LOOP_BREAK_LABEL, merge_block);
      _metadata.add(LOOP_CONTINUE_LABEL, cond_block);

      b().CreateBr(loop_block);
      b().SetInsertPoint(loop_block);
      break;
    }

    default: {}
  }
}

void IrGenerator::infix(const Node& node, const result_list& results)
{
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
        b().CreateCondBr(i2b(results[0]), then_block,
                         has_else ? else_block : merge_block);
        b().SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        b().CreateBr(merge_block);
        b().SetInsertPoint(else_block);
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
        b().CreateBr(cond_block);
        b().SetInsertPoint(cond_block);
      }
      if (results.size() == 2) {
        b().CreateCondBr(i2b(results[1]), loop_block, merge_block);
        b().SetInsertPoint(after_block);
      }
      if (results.size() == 3) {
        b().CreateBr(cond_block);
        b().SetInsertPoint(loop_block);
      }
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      auto cond_block = (llvm::BasicBlock*)_metadata[LOOP_COND_BLOCK];
      b().CreateBr(cond_block);
      b().SetInsertPoint(cond_block);
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

        b().CreateCondBr(i2b(results[0]), then_block, else_block);
        b().SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
        auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
        b().CreateBr(merge_block);

        // Metadata blocks must always be updated to the current one, in case we
        // created a bunch of new ones before we got to the end!
        _metadata[IF_THEN_BLOCK] = b().GetInsertBlock();
        b().SetInsertPoint(else_block);
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
      _metadata.add(LOGICAL_OP_SOURCE_BLOCK, b().GetInsertPoint());
      auto rhs_block = create_block(LOGICAL_OP_RHS_BLOCK, "rhs");
      auto merge_block = create_block(MERGE_BLOCK, "merge");

      if (node.type == Node::LOGICAL_OR) {
        b().CreateCondBr(i2b(results[0]), merge_block, rhs_block);
      }
      else {
        b().CreateCondBr(i2b(results[0]), rhs_block, merge_block);
      }
      b().SetInsertPoint(rhs_block);
      break;
    }

    default: {}
  }
}

IrGeneratorUnion IrGenerator::visit(const Node& node,
                                    const result_list& results)
{
  auto parent = b().GetInsertBlock() ?
      b().GetInsertBlock()->getParent() : nullptr;
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
      return type == Node::LOGICAL_OR ? b().CreateOr(v, u, "lor") :
             type == Node::LOGICAL_AND ? b().CreateAnd(v, u, "land") :
             type == Node::BITWISE_OR ? b().CreateOr(v, u, "or") :
             type == Node::BITWISE_AND ? b().CreateAnd(v, u, "and") :
             type == Node::BITWISE_XOR ? b().CreateXor(v, u, "xor") :
             type == Node::BITWISE_LSHIFT ? b().CreateShl(v, u, "lsh") :
             type == Node::BITWISE_RSHIFT ? b().CreateAShr(v, u, "rsh") :
             type == Node::POW ? pow(v, u) :
             type == Node::MOD ? mod(v, u) :
             type == Node::ADD ? b().CreateAdd(v, u, "add") :
             type == Node::SUB ? b().CreateSub(v, u, "sub") :
             type == Node::MUL ? b().CreateMul(v, u, "mul") :
             type == Node::DIV ? div(v, u) :
             type == Node::EQ ? b().CreateICmpEQ(v, u, "eq") :
             type == Node::NE ? b().CreateICmpNE(v, u, "ne") :
             type == Node::GE ? b().CreateICmpSGE(v, u, "ge") :
             type == Node::LE ? b().CreateICmpSLE(v, u, "le") :
             type == Node::GT ? b().CreateICmpSGT(v, u, "gt") :
             type == Node::LT ? b().CreateICmpSLT(v, u, "lt") :
             nullptr;
    }
    return type == Node::POW ? pow(v, u) :
           type == Node::MOD ? mod(v, u) :
           type == Node::ADD ? b().CreateFAdd(v, u, "fadd") :
           type == Node::SUB ? b().CreateFSub(v, u, "fsub") :
           type == Node::MUL ? b().CreateFMul(v, u, "fmul") :
           type == Node::DIV ? div(v, u) :
           type == Node::EQ ? b().CreateFCmpOEQ(v, u, "feq") :
           type == Node::NE ? b().CreateFCmpONE(v, u, "fne") :
           type == Node::GE ? b().CreateFCmpOGE(v, u, "fge") :
           type == Node::LE ? b().CreateFCmpOLE(v, u, "fle") :
           type == Node::GT ? b().CreateFCmpOGT(v, u, "fgt") :
           type == Node::LT ? b().CreateFCmpOLT(v, u, "flt") :
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
      args.push_back(void_ptr_type());
      return generic_function_type(results[0], args);
    }

    case Node::GLOBAL:
      dereference_scoped_locals(true);
      if (!node.static_info.closed_environment.empty()) {
        _scope_closures.pop_back();
      }
      b().CreateRetVoid();
      _symbol_table.pop();
      _metadata.pop();
      _refcount_locals.pop_back();
      return results[0];
    case Node::GLOBAL_ASSIGN:
    {
      auto function = (llvm::Function*)parent;
      auto function_type =
          (llvm::FunctionType*)function->getType()->getPointerElementType();
      get_trampoline_function(get_yang_type(function_type));
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
        dereference_scoped_locals(true);
        b().CreateRetVoid();
      }
      else {
        // In a function that passes the static check, control never reaches
        // this point; but the block must have a terminator.
        b().CreateBr(b().GetInsertBlock());
      }
      auto parent_block = (llvm::BasicBlock*)_metadata[PARENT_BLOCK];

      if (!node.static_info.closed_environment.empty()) {
        _scope_closures.pop_back();
      }
      _symbol_table.pop();
      _symbol_table.pop();
      _metadata.pop();
      // After metadata pop, function created closure iff closure pointer
      // exists.
      if (_metadata[CLOSURE_PTR]) {
        --_function_scope;
        _scope_to_function_map.erase(--_scope_to_function_map.end());
      }
      _refcount_locals.pop_back();
      if (!_metadata.has(FUNCTION)) {
        return parent;
      }
      // If this was a nested function, set the insert point back to the last
      // block in the enclosing function and make the proper expression value.
      b().SetInsertPoint(parent_block);
      // If the current function created a closure, we have to pass the new
      // environment pointer (which has a parent pointer to the old one)
      // instead. There is some potential for optimisation if an entire tree
      // of inner functions never creates a closure, where we could instead
      // dereference and pass only the closure scopes they actually access; it's
      // kind of complicated and not really a huge optimisation, though.
      auto closure = _metadata[CLOSURE_PTR];
      return generic_function_value(
          parent, closure ? closure : _metadata[ENVIRONMENT_PTR]);
    }
    case Node::NAMED_EXPRESSION:
      return results[0];
      break;

    case Node::BLOCK:
    {
      auto after_block =
          llvm::BasicBlock::Create(b().getContext(), "after", parent);
      b().CreateBr(after_block);
      b().SetInsertPoint(after_block);
      _symbol_table.pop();
      dereference_scoped_locals(false);
      _refcount_locals.back().pop_back();
      return parent;
    }
    case Node::EMPTY_STMT:
      return constant_int(0);
    case Node::EXPR_STMT:
      return results[0];
    case Node::RETURN_VOID_STMT:
    case Node::RETURN_STMT:
    {
      auto dead_block =
          llvm::BasicBlock::Create(b().getContext(), "dead", parent);
      dereference_scoped_locals(true);
      llvm::Value* v = node.type == Node::RETURN_STMT ?
          b().CreateRet(results[0]) : b().CreateRetVoid();
      b().SetInsertPoint(dead_block);
      return v;
    }
    case Node::IF_STMT:
    {
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      b().CreateBr(merge_block);
      b().SetInsertPoint(merge_block);
      dereference_scoped_locals(false);
      _refcount_locals.back().pop_back();
      return results[0];
    }
    case Node::FOR_STMT:
    {
      auto after_block = (llvm::BasicBlock*)_metadata[LOOP_AFTER_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      b().CreateBr(after_block);
      b().SetInsertPoint(merge_block);
      dereference_scoped_locals(false);
      _refcount_locals.back().pop_back();
      return results[0];
    }
    case Node::DO_WHILE_STMT:
    {
      auto loop_block = (llvm::BasicBlock*)_metadata[LOOP_BODY_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      b().CreateCondBr(i2b(results[1]), loop_block, merge_block);
      b().SetInsertPoint(merge_block);
      dereference_scoped_locals(false);
      _refcount_locals.back().pop_back();
      return results[0];
    }
    case Node::BREAK_STMT:
    case Node::CONTINUE_STMT:
    {
      auto dead_block =
          llvm::BasicBlock::Create(b().getContext(), "dead", parent);
      llvm::Value* v = b().CreateBr((llvm::BasicBlock*)_metadata[
          node.type == Node::BREAK_STMT ? LOOP_BREAK_LABEL :
                                          LOOP_CONTINUE_LABEL]);
      b().SetInsertPoint(dead_block);
      return v;
    }

    case Node::SCOPE_RESOLUTION:
    {
      // Scope resolution always refers to a context function.
      std::string s =
          node.static_info.user_type_name + "::" + node.string_value;
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
        return memory_load(get_variable_ptr(node.string_value));
      }
      // Otherwise it's a top-level function, global, or context function. We
      // must be careful to only return globals/functions *after* they have been
      // defined, otherwise it might be a reference to a context function of the
      // same name.
      if (_symbol_table.has(node.string_value)) {
        // If the symbol table entry is non-null it's a top-level function; just
        // get the value.
        if (_symbol_table.get(node.string_value, 0)) {
          return generic_function_value(
              _symbol_table.get(node.string_value, 0), global_ptr());
        }
        // Otherwise it's a global, so look up in the global structure with a
        // byte-aligned load.
        return memory_load(global_ptr(node.string_value));
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
        return b().CreateSelect(i2b(results[0]), results[1], results[2]);
      }
      // Update in case we branched again.
      _metadata[IF_ELSE_BLOCK] = b().GetInsertBlock();

      auto then_block = (llvm::BasicBlock*)_metadata[IF_THEN_BLOCK];
      auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();
      b().CreateBr(merge_block);
      b().SetInsertPoint(merge_block);
      auto phi = b().CreatePHI(types[1], 2, "tern");
      phi->addIncoming(results[1], then_block);
      phi->addIncoming(results[2], else_block);
      return phi;
    }
    case Node::CALL:
    {
      if (_metadata.has(TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }

      // TODO: is it true that always either environment or target pointer is
      // null? If so, can we get rid of get_function_type_with_target and the
      // branching here, and just call with the final environment/target
      // pointers OR-ed together?
      // In fact, it occurs to me that since we always know the type statically,
      // there isn't actually any need to pass the trampoline around. Instead of
      // (function/trampoline, environment, target) we could simply use
      // (target, environment). The logic becomes: do a runtime check to see if
      // environment is null. If so, call trampoline looked up from
      // _reverse_trampoline_map; otherwise, call target directly and pass
      // environment.
      // We can then get rid of the global Yang reverse trampolines, but we
      // still need global Yang *forward* trampolines, I think (to call Yang
      // functions from C++), and we maybe don't need per-module forward
      // trampolines any more either. That seems to make sense: global forward
      // trampolines, and per-module reverse trampolines only.
      std::vector<llvm::Value*> args;
      llvm::Value* genf = results[0];

      // Special logic for member-function calling.
      // TODO: get rid of this now that we have closures.
      if (node.children[0]->type == Node::MEMBER_SELECTION) {
        std::string s = node.children[0]->static_info.user_type_name +
            "::" + node.children[0]->string_value;
        auto it = _context.get_functions().find(s);
        genf = generic_function_value(it->second);

        args.push_back(results[0]);
      }
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i]);
      }

      // Extract pointers from the struct.
      llvm::Value* fptr = b().CreateExtractValue(genf, 0, "fptr");
      llvm::Value* eptr = b().CreateExtractValue(genf, 1, "eptr");
      llvm::Value* tptr = b().CreateExtractValue(genf, 2, "tptr");
      args.push_back(eptr);

      // Switch on the presence of a trampoline function pointer.
      auto cpp_block =
          llvm::BasicBlock::Create(b().getContext(), "cpp", parent);
      auto yang_block =
          llvm::BasicBlock::Create(b().getContext(), "yang", parent);
      auto merge_block =
          llvm::BasicBlock::Create(b().getContext(), "merge", parent);

      auto f_type = get_function_type_with_target(genf->getType());
      auto fp_type = llvm::PointerType::get(f_type, 0);

      llvm::Value* cmp = b().CreateICmpNE(
          tptr, llvm::ConstantPointerNull::get(void_ptr_type()));
      b().CreateCondBr(cmp, cpp_block, yang_block);

      b().SetInsertPoint(yang_block);
      llvm::Value* yang_val = b().CreateCall(fptr, args);
      b().CreateBr(merge_block);

      args.push_back(tptr);
      b().SetInsertPoint(cpp_block);
      llvm::Value* cpp_val =
          b().CreateCall(b().CreateBitCast(fptr, fp_type, "cast"), args);
      b().CreateBr(merge_block);

      b().SetInsertPoint(merge_block);
      if (!f_type->getReturnType()->isVoidTy()) {
        auto phi = b().CreatePHI(f_type->getReturnType(), 2, "call");
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
      // Update in case we branched again.
      _metadata[LOGICAL_OP_RHS_BLOCK] = b().GetInsertBlock();

      auto source_block = (llvm::BasicBlock*)_metadata[LOGICAL_OP_SOURCE_BLOCK];
      auto rhs_block = (llvm::BasicBlock*)_metadata[LOGICAL_OP_RHS_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();

      auto rhs = b2i(i2b(results[1]));
      b().CreateBr(merge_block);
      b().SetInsertPoint(merge_block);
      llvm::Constant* constant =
          constant_int(node.type == Node::LOGICAL_OR ? 1 : 0);
      llvm::Type* type = int_type();
      if (types[1]->isVectorTy()) {
        std::size_t n = types[1]->getVectorNumElements();
        constant = constant_vector(constant, n);
        type = vector_type(type, n);
      }

      auto phi = b().CreatePHI(type, 2, "cut");
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
      return b2i(b().CreateICmpEQ(results[0], cmp, "lneg"));
    }
    case Node::BITWISE_NEGATION:
    {
      llvm::Constant* cmp = constant_int(0u - 1);
      if (types[0]->isVectorTy()) {
        cmp = constant_vector(cmp, types[0]->getVectorNumElements());
      }
      return b().CreateXor(results[0], cmp, "neg");
    }
    case Node::ARITHMETIC_NEGATION:
    {
      llvm::Constant* cmp = types[0]->isIntOrIntVectorTy() ?
          constant_int(0) : constant_float(0);
      if (types[0]->isVectorTy()) {
        cmp = constant_vector(cmp, types[0]->getVectorNumElements());
      }
      return types[0]->isIntOrIntVectorTy() ?
          b().CreateSub(cmp, results[0], "sub") :
          b().CreateFSub(cmp, results[0], "fsub");
    }

    case Node::ASSIGN:
    {
      // See Node::IDENTIFIER.
      const std::string& s = node.children[0]->string_value;
      if (_symbol_table[s]) {
        memory_store(results[1], get_variable_ptr(s));
      }
      // We can't store values for globals in the symbol table since the lookup
      // depends on the current function's environment pointer argument.
      else {
        memory_store(results[1], global_ptr(s));
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
      if (_metadata.has(GLOBAL_INIT_FUNCTION) && _symbol_table.size() <= 3) {
        _symbol_table.add(s, 0, nullptr);
        memory_store(results[1], global_ptr(s));
        return results[1];
      }

      const std::string& unique_name =
          s + "/" + std::to_string(node.static_info.scope_number);

      llvm::Value* storage = nullptr;
      if (_symbol_table.has(unique_name)) {
        storage = _symbol_table[unique_name];
        _value_to_unique_name_map.emplace(storage, unique_name);
      }
      else {
        // Optimisation passes such as mem2reg work much better when memory
        // locations are declared in the entry block (so they are guaranteed to
        // execute once).
        auto& entry_block =
            ((llvm::Function*)b().GetInsertPoint()->getParent())->getEntryBlock();
        llvm::IRBuilder<> entry(&entry_block, entry_block.begin());
        storage = entry.CreateAlloca(types[1], nullptr, s);
        memory_init(entry, storage);
      }

      memory_store(results[1], storage);
      _symbol_table.add(s, storage);
      _refcount_locals.back().back().push_back(storage);
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
        v = b().CreateInsertElement(v, results[i], constant_int(i), "vec");
      }
      return v;
    }
    case Node::VECTOR_INDEX:
    {
      // Indexing out-of-bounds produces constant zero.
      llvm::Constant* zero = types[0]->isIntOrIntVectorTy() ?
          constant_int(0) : constant_float(0);
      auto ge = b().CreateICmpSGE(results[1], constant_int(0), "idx");
      auto lt = b().CreateICmpSLT(
          results[1], constant_int(types[0]->getVectorNumElements()), "idx");

      auto in = b().CreateAnd(ge, lt, "idx");
      return b().CreateSelect(
          in, b().CreateExtractElement(results[0], results[1], "idx"),
          zero, "idx");
    }

    default:
      return (llvm::Value*)nullptr;
  }
}

void IrGenerator::init_structure_type(
    llvm::Type*& output_type, structure_numbering& output_numbering,
    const symbol_frame& symbols, const std::string& name) const
{
  std::vector<llvm::Type*> type_list;
  // The first element in any structure is always some pointer. For the global
  // data structure it's a pointer to Yang program instance with which the data
  // is associated; for closure structures it's a pointer to the parent
  // structure.
  // TODO: do we even need the Yang instance pointer any more?
  // TODO: also, global data structure should now be refcounted like closure
  // structures instead of destroyed when the Instance is!
  type_list.push_back(void_ptr_type());
  std::size_t number = 1;
  // Since the global data structure may contain pointers to functions which
  // themselves take said implicit argument, the global_data variable must be
  // set (to an opaque type) before we call get_llvm_type.
  auto struct_type =
      llvm::StructType::create(_module.getContext(), name);
  output_type = llvm::PointerType::get(struct_type, 0);
  for (const auto& pair : symbols) {
    // Type-calculation must be kept up-to-date with new types, or globals of
    // that type will fail.
    llvm::Type* t = get_llvm_type(pair.second);
    type_list.push_back(t);
    output_numbering[pair.first] = number++;
  }
  struct_type->setBody(type_list, false);
}

llvm::Value* IrGenerator::allocate_structure_value(
    llvm::Type* type, const structure_numbering& numbering)
{
  llvm::Type* llvm_size_t =
      llvm::IntegerType::get(b().getContext(), 8 * sizeof(std::size_t));

  auto malloc_ptr = get_native_function(
      "malloc", (yang::void_fp)&malloc,
      llvm::FunctionType::get(type, llvm_size_t, false));

  // Compute sizeof(type) by indexing one past the null pointer.
  llvm::Value* size_of = b().CreateIntToPtr(constant_int(0), type, "null");
  size_of = b().CreateGEP(size_of, constant_int(1), "sizeof");
  size_of = b().CreatePtrToInt(size_of, llvm_size_t, "size");

  // Call malloc, make sure refcounted memory is initialised, and return the
  // pointer.
  llvm::Value* v = b().CreateCall(malloc_ptr, size_of, "call");
  for (const auto& pair : numbering) {
    memory_init(b(), structure_ptr(v, pair.second));
  }
  return v;
}

llvm::Value* IrGenerator::get_parent_struct(
    std::size_t parent_steps, llvm::Value* v)
{
  llvm::Type* void_ptr_ptr = llvm::PointerType::get(
      llvm::StructType::create(b().getContext(), void_ptr_type()), 0);
  for (std::size_t i = 0; i < parent_steps; ++i) {
    v = memory_load(structure_ptr(b().CreateBitCast(v, void_ptr_ptr), 0));
  }
  return v;
}

llvm::Value* IrGenerator::get_variable_ptr(const std::string& name)
{
  // See notes in header file about the flow here.
  auto it = _scope_to_function_map.upper_bound(_symbol_table.index(name));
  llvm::Value* v = _symbol_table[name];

  if (it == _scope_to_function_map.begin() ||
      it == _scope_to_function_map.end()) {
    return v;
  }

  // These indices now must be different.
  std::size_t closure_index = (--it)->second;
  std::size_t current_index = _scope_to_function_map.rbegin()->second;

  const std::string& unique_name = _value_to_unique_name_map[v];
  llvm::Value* closure_ptr = get_parent_struct(
      current_index - closure_index - 1, _metadata[ENVIRONMENT_PTR]);
  llvm::Type* closure_type = _scope_closures[closure_index].type;
  std::size_t struct_index =
      _scope_closures[closure_index].numbering[unique_name];
  return structure_ptr(
      b().CreateBitCast(closure_ptr, closure_type), struct_index);
}

llvm::Value* IrGenerator::allocate_closure_struct(
    const symbol_frame& symbols, llvm::Value* parent_ptr)
{
  if (symbols.empty()) {
    // Store a nullptr to be explicit.
    _metadata.add(CLOSURE_PTR, nullptr);
    return nullptr;
  }

  // Handle closure-structure initialisation.
  llvm::Type* closure_type = nullptr;
  structure_numbering closure_numbering;
  init_structure_type(closure_type, closure_numbering, symbols, "closure");
  llvm::Value* closure_value =
      allocate_structure_value(closure_type, closure_numbering);
  // Store in metadata.
  _metadata.add(CLOSURE_PTR, closure_value);
  _scope_closures.push_back({closure_type, closure_numbering});

  // Store parent pointer in the first slot.
  memory_store(b().CreateBitCast(parent_ptr, void_ptr_type()),
               structure_ptr(closure_value, 0));
  // Set up the symbol-table for all the rest. If a closed variable "v"
  // appears in scope #1, for instance, we store "v/1" in the symbol table
  // with a pointer into the closure-structure.
  //
  // Then, when we reach the actual declaration of v in scope #1, we store
  // into "v/1" and copy the symbol table entry for "v/1" to "v" for that
  // scope and its children (as in the argument list code below).
  for (const auto& pair : symbols) {
    llvm::Value* ptr =
        structure_ptr(closure_value, closure_numbering[pair.first]);
    _symbol_table.add(pair.first, ptr);
  }
  return closure_value;
}

void IrGenerator::create_function(
    const Node& node, llvm::FunctionType* function_type)
{
  _metadata.push();

  // Linkage will be set later if necessary.
  auto function = llvm::Function::Create(
      function_type, llvm::Function::InternalLinkage,
      "anonymous", &_module);

  // The code for Node::TYPE_FUNCTION in visit() ensures it takes an environment
  // pointer.
  // TODO: we possibly need to do refcounting on the special function arguments
  // (environment pointer; self-recursion pointer...)?
  auto eptr = --function->arg_end();
  eptr->setName("env");
  _metadata.add(ENVIRONMENT_PTR, eptr);
  _metadata.add(FUNCTION, function);
  _metadata.add(PARENT_BLOCK, b().GetInsertPoint());

  auto block = llvm::BasicBlock::Create(b().getContext(), "entry", function);
  b().SetInsertPoint(block);

  _symbol_table.push();
  // Store the function's name in its own scope. Recursive lookup handled
  // similarly to arguments below.
  if (_immediate_left_assign.length()) {
    llvm::Type* fp_type = llvm::PointerType::get(function_type, 0);
    llvm::Value* v = b().CreateAlloca(
        generic_function_type(fp_type), nullptr, _immediate_left_assign);
    b().CreateStore(generic_function_value(function, eptr), v);
    _symbol_table.add(_immediate_left_assign, v);
    _immediate_left_assign.clear();
  }
  // Increase depth of scope pointers if the parent function created a closure.
  if (_metadata[CLOSURE_PTR]) {
    _scope_to_function_map.emplace(_symbol_table.size(), ++_function_scope);
  }
  _symbol_table.push();
  // Some optimisations may be possible. For example, const variables may not
  // need to go in the closure structure. Maybe LLVM can handle that anyway?
  allocate_closure_struct(node.static_info.closed_environment, eptr);

  // Set up the arguments.
  std::size_t arg_num = 0;
  for (auto it = function->arg_begin();
       it != --function->arg_end(); ++it, ++arg_num) {
    const std::string& name =
        node.children[0]->children[1 + arg_num]->string_value;
    const std::string& unique_name =
        name + "/" + std::to_string(node.static_info.scope_number);

    llvm::Value* storage = nullptr;
    if (_symbol_table.has(unique_name)) {
      storage = _symbol_table[unique_name];
      _value_to_unique_name_map.emplace(storage, unique_name);
    }
    else {
      // Rather than reference argument values directly, we create an alloca
      // and store the argument in there. This simplifies things, since we
      // can emit the same IR code when referencing local variables or
      // function arguments.
      storage = b().CreateAlloca(
          function_type->getParamType(arg_num), nullptr, name);
      memory_init(b(), storage);
    }

    // It's possible refcounting isn't necessary on arguments, since they're
    // const and will usually be referenced somewhere up the call stack. I'm not
    // convinced, though.
    memory_store(it, storage);
    _symbol_table.add(name, storage);
    _refcount_locals.back().back().push_back(storage);
  }
}

llvm::Value* IrGenerator::i2b(llvm::Value* v)
{
  llvm::Constant* cmp = constant_int(0);
  if (v->getType()->isVectorTy()) {
    cmp = constant_vector(cmp, v->getType()->getVectorNumElements());
  }
  return b().CreateICmpNE(v, cmp, "bool");
}

llvm::Value* IrGenerator::b2i(llvm::Value* v)
{
  llvm::Type* type = int_type();
  if (v->getType()->isVectorTy()) {
    type = vector_type(type, v->getType()->getVectorNumElements());
  }
  return b().CreateZExt(v, type, "int");
}

llvm::Value* IrGenerator::i2w(llvm::Value* v)
{
  llvm::Type* type = float_type();
  if (v->getType()->isVectorTy()) {
    type = vector_type(type, v->getType()->getVectorNumElements());
  }
  return b().CreateSIToFP(v, type, "wrld");
}

llvm::Value* IrGenerator::w2i(llvm::Value* v)
{
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
  auto cast = b().CreateFPToSI(v, type, "int");
  auto back = b().CreateSIToFP(cast, back_type, "int");

  auto a_check = b().CreateFCmpOLT(v, zero, "int");
  auto b_check = b().CreateFCmpONE(v, back, "int");
  return b().CreateSub(cast,
      b2i(b().CreateAnd(a_check, b_check, "int")), "int");
}

llvm::Value* IrGenerator::structure_ptr(llvm::Value* ptr, std::size_t index)
{
  // The first index indexes the structure data pointer itself, i.e. to obtain
  // the one and only global data structure at that memory location.
  std::vector<llvm::Value*> indexes{constant_int(0), constant_int(index)};
  llvm::Value* v = b().CreateGEP(ptr, indexes, "index");
  return v;
}

llvm::Value* IrGenerator::global_ptr(const std::string& name)
{
  return structure_ptr(global_ptr(), _global_numbering[name]);
}

llvm::Value* IrGenerator::global_ptr()
{
  // This always gets the global pointer, even in inner functions of closure
  // scopes.
  llvm::Value* ptr = get_parent_struct(
      _scope_to_function_map.rbegin()->second, _metadata[ENVIRONMENT_PTR]);
  // Bitcast, since it's represented as void pointer.
  return b().CreateBitCast(ptr, _global_data);
}

llvm::Value* IrGenerator::memory_load(llvm::Value* ptr)
{
  return b().CreateAlignedLoad(ptr, 1, "load");
}

void IrGenerator::memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr)
{
  // We need to make sure ref-counted memory locations are initialised with
  // something sensible. Otherwise, the first store will try to decrement
  // the refcount on something undefined. (Important in particular for variable
  // declarations which are "executed" more than once.)
  llvm::Type* elem = ptr->getType()->getPointerElementType();
  if (elem->isStructTy()) {
    // Null Yang function.
    pos.CreateAlignedStore(
        generic_function_value_null((llvm::StructType*)elem), ptr, 1);
  }
}

void IrGenerator::memory_store(llvm::Value* value, llvm::Value* ptr)
{
  llvm::Value* old = memory_load(ptr);
  update_reference_count(old, -1);
  update_reference_count(value, 1);
  b().CreateAlignedStore(value, ptr, 1);
}

void IrGenerator::update_reference_count(llvm::Value* value, int_t change)
{
  if (!value->getType()->isStructTy()) {
    return;
  }
  llvm::Value* tptr = b().CreateExtractValue(value, 2, "tptr");

  auto parent = b().GetInsertBlock()->getParent();
  auto refcount_block =
      llvm::BasicBlock::Create(b().getContext(), "refcount", parent);
  auto merge_block =
      llvm::BasicBlock::Create(b().getContext(), "merge", parent);

  llvm::Value* cmp = b().CreateICmpNE(
      tptr, llvm::ConstantPointerNull::get(void_ptr_type()));
  b().CreateCondBr(cmp, refcount_block, merge_block);

  b().SetInsertPoint(refcount_block);
  std::vector<llvm::Value*> args{tptr, constant_int(change)};
  b().CreateCall(_refcount_function, args);
  b().CreateBr(merge_block);
  b().SetInsertPoint(merge_block);
}

void IrGenerator::dereference_scoped_locals(bool all_scopes)
{
  const auto& function = _refcount_locals.back();
  if (all_scopes) {
    for (const auto& scope : function) {
      for (llvm::Value* alloc : scope) {
        llvm::Value* v = memory_load(alloc);
        update_reference_count(v, -1);
        memory_init(b(), alloc);
      }
    }
    return;
  }
  for (llvm::Value* alloc : function.back()) {
    llvm::Value* v = memory_load(alloc);
    update_reference_count(v, -1);
    memory_init(b(), alloc);
  }
}

llvm::Value* IrGenerator::pow(llvm::Value* v, llvm::Value* u)
{
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
    llvm::Value* r = b().CreateCall(pow_ptr, args, "pow");
    return t->isIntOrIntVectorTy() ? w2i(r) : r;
  }

  llvm::Value* result =
      constant_vector(constant_float(0), t->getVectorNumElements());
  for (std::size_t i = 0; i < t->getVectorNumElements(); ++i) {
    llvm::Value* x = b().CreateExtractElement(v, constant_int(i), "pow");
    llvm::Value* y = b().CreateExtractElement(u, constant_int(i), "pow");
    std::vector<llvm::Value*> args{x, y};

    llvm::Value* call = b().CreateCall(pow_ptr, args, "pow");
    result = b().CreateInsertElement(result, call, constant_int(i), "pow");
  }
  return t->isIntOrIntVectorTy() ? w2i(result) : result;
}

llvm::Value* IrGenerator::mod(llvm::Value* v, llvm::Value* u)
{
  if (!v->getType()->isIntOrIntVectorTy()) {
    return b().CreateFRem(v, u, "fmod");
  }

  // Implements the following algorithm:
  // return (v >= 0 ? v : v + (bool(|v| % |u|) + |v| / |u|) * |u|) % |u|;
  // There are simpler ways, but they are vulnerable to overflow errors.
  // k = |v| % |u| + |v| / |u| is the smallest postive integer such that
  // k * |u| >= |v|.
  auto v_check = b().CreateICmpSGE(v, constant_int(0), "mod");
  auto u_check = b().CreateICmpSGE(u, constant_int(0), "mod");
  auto v_abs = b().CreateSelect(
      v_check, v, b().CreateSub(constant_int(0), v, "mod"), "mod");
  auto u_abs = b().CreateSelect(
      u_check, u, b().CreateSub(constant_int(0), u, "mod"), "mod");

  auto k = b().CreateAdd(b2i(i2b(b().CreateSRem(v_abs, u_abs, "mod"))),
                         b().CreateSDiv(v_abs, u_abs, "mod"), "mod");
  auto lhs = b().CreateSelect(
      v_check, v,
      b().CreateAdd(v, b().CreateMul(k, u_abs, "mod"), "mod"), "mod");
  return b().CreateSRem(lhs, u_abs, "mod");
}

llvm::Value* IrGenerator::div(llvm::Value* v, llvm::Value* u)
{
  if (!v->getType()->isIntOrIntVectorTy()) {
    return b().CreateFDiv(v, u, "fdiv");
  }

  // Implements the following algorithm:
  // bool sign = (v < 0) == (u < 0);
  // int t = (v < 0 ? -(1 + v) : v) / |u|;
  // return (sign ? t : -(1 + t)) + (u < 0);
  auto v_check = b().CreateICmpSLT(v, constant_int(0), "div");
  auto u_check = b().CreateICmpSLT(u, constant_int(0), "div");
  auto sign = b().CreateICmpEQ(v_check, u_check, "div");
  auto u_abs = b().CreateSelect(
      u_check, b().CreateSub(constant_int(0), u, "div"), u, "div");

  auto t = b().CreateSelect(
      v_check,
      b().CreateSub(constant_int(-1), v, "div"), v, "div");
  t = b().CreateSDiv(t, u_abs, "div");
  return b().CreateAdd(
      b().CreateSelect(sign, t,
                       b().CreateSub(constant_int(-1), t, "div"), "div"),
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
    v = b().CreateInsertElement(
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
    llvm::Value* v = b().CreateExtractElement(value, constant_int(i), "idx");
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
    v = b().CreateAnd(v, comparisons[i], "fold");
  }
  return v;
}

llvm::BasicBlock* IrGenerator::create_block(
    metadata meta, const std::string& name)
{
  auto parent = b().GetInsertBlock() ?
      b().GetInsertBlock()->getParent() : nullptr;
  auto block = llvm::BasicBlock::Create(b().getContext(), name, parent);
  _metadata.add(meta, block);
  return block;
}

// End namespace yang::internal.
}
}
