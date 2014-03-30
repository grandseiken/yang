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

// TODO: using LLVM frem, lsh, rsh operations seems to break sometimes when
// one operand is e.g. loaded from a data structure. I don't understand why.
// Maybe we can use an inlining IR function?
namespace {
  yang::int_t lsh(yang::uint_t a, yang::uint_t b)
  {
    return a << b;
  }

  yang::int_t rsh(yang::uint_t a, yang::uint_t b)
  {
    return a >> b;
  }
}

namespace yang {
namespace internal {

IrGenerator::IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
                         symbol_frame& globals, const Context& context)
  : IrCommon(module, engine)
  , _context(context)
  , _member_function_closure(_b)
{
  _scopes.emplace_back(_b);
  // Set up the global data type. Since each program is designed to support
  // multiple independent instances running simultaeneously, the idea is to
  // define a structure type with a field for each global variable. Each
  // function will take a pointer to the global data structure as an implicit
  // final parameter.
  _scopes[0].init_structure_type(globals, true);
  // Suppose a user type T has a member R(A...) f. So that an expression t.f
  // can be used as a generic function value, it creates an implicit closure;
  // that is, "val = t.f;" is equivalent to:
  // const bind = t; val = R(A... a) {return T::f(bind, a...);};
  // Rather than transforming the AST to do this, we do it manually: use a
  // single closure structure type with just a void pointer, and transform t.f
  // to (mem[T::f], env_mem(t)).
  symbol_frame user_type;
  user_type.emplace("object", yang::Type::user_t());
  _member_function_closure.init_structure_type(user_type, false, false);

  // We need to generate a reverse trampoline function for each function in the
  // Context. User type member functions are present in the context function map
  // as well as free functions.
  for (const auto& pair : context.get_functions()) {
    get_reverse_trampoline_function(pair.second.type, false);
  }
}

void IrGenerator::emit_global_functions()
{
  auto& scope = _scopes[0];
  // Create allocator function. It takes a pointer to the Yang program instance.
  auto alloc = llvm::Function::Create(
      llvm::FunctionType::get(scope.structure.type, false),
      llvm::Function::ExternalLinkage, "!global_alloc", &_b.module);
  auto alloc_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", alloc);
  _b.b.SetInsertPoint(alloc_block);

  // TODO: here we call the global initialisation functions. It should also be
  // possible to define custom destructor functions that are called when the
  // global structure is freed.
  llvm::Value* v = scope.allocate_structure_value();
  for (llvm::Function* f : _global_inits) {
    _b.b.CreateCall(f, v);
  }
  _b.b.CreateRet(v);

  // Create accessor functions for each field of the global structure.
  for (const auto pair : scope.structure.table) {
    llvm::Type* t = _b.get_llvm_type(pair.second.type);

    std::string name = "!global_get_" + pair.first;
    auto function_type =
        llvm::FunctionType::get(t, scope.structure.type, false);
    auto getter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_b.module);
    get_trampoline_function(
        yang::Type::function_t(pair.second.type, {}), false);

    auto getter_block = llvm::BasicBlock::Create(
        _b.b.getContext(), "entry", getter);
    auto it = getter->arg_begin();
    it->setName("global");
    _b.b.SetInsertPoint(getter_block);
    _b.b.CreateRet(scope.memory_load(
        pair.second.type, scope.structure_ptr(&*it, pair.second.index)));

    name = "!global_set_" + pair.first;
    std::vector<llvm::Type*> setter_args{t, scope.structure.type};
    function_type = llvm::FunctionType::get(_b.void_type(), setter_args, false);
    auto setter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_b.module);
    get_trampoline_function(
        yang::Type::function_t(yang::Type::void_t(), {pair.second.type}),
        false);

    auto setter_block = llvm::BasicBlock::Create(
        _b.b.getContext(), "entry", setter);
    it = setter->arg_begin();
    it->setName("value");
    auto jt = it;
    ++jt;
    jt->setName("global");
    _b.b.SetInsertPoint(setter_block);
    scope.memory_store(Value(pair.second.type, &*it),
                       scope.structure_ptr(&*jt, pair.second.index));
    _b.b.CreateRetVoid();
  }
}

void IrGenerator::preorder(const Node& node)
{
  auto& fback = _scopes.back();
  switch (node.type) {
    case Node::GLOBAL:
    {
      // GLOBAL init functions don't need external linkage, since they are
      // called automatically by the externally-visible global structure
      // allocation function.
      auto function = llvm::Function::Create(
          llvm::FunctionType::get(_b.void_type(),
                                  _scopes[0].structure.type, false),
          llvm::Function::InternalLinkage, "!global_init", &_b.module);
      _global_inits.push_back(function);
      auto block =
          llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);

      // Store a special entry in the symbol table for the implicit global
      // structure pointer.
      auto it = function->arg_begin();
      it->setName("env");
      _scopes.emplace_back(_scopes.back().next_lex_scope());
      auto& fback = _scopes.back();
      _b.b.SetInsertPoint(block);

      fback.metadata.add(LexScope::ENVIRONMENT_PTR, &*it);
      fback.metadata.add(LexScope::FUNCTION, function);
      if (!node.static_info.closed_environment.empty()) { 
        fback.init_structure_type(node.static_info.closed_environment, false);
        llvm::Value* v = fback.allocate_closure_struct(&*it);
        fback.metadata[LexScope::CLOSURE_PTR] = v;
        fback.update_reference_count(nullptr, v, 1);
      }

      fback.metadata.add(LexScope::GLOBAL_INIT_FUNCTION, function);
      break;
    }

    case Node::FUNCTION:
      fback.metadata.push();
      fback.metadata.add(LexScope::TYPE_EXPR_CONTEXT, nullptr);
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
      fback.push_scope();
      break;

    case Node::IF_STMT:
    {
      fback.push_scope();
      fback.create_block(LexScope::IF_THEN_BLOCK, "then");
      fback.create_block(LexScope::MERGE_BLOCK, "merge");
      if (node.children.size() > 2) {
        fback.create_block(LexScope::IF_ELSE_BLOCK, "else");
      }
      break;
    }

    case Node::FOR_STMT:
    {
      fback.push_scope();
      fback.create_block(LexScope::LOOP_COND_BLOCK, "cond");
      fback.create_block(LexScope::LOOP_BODY_BLOCK, "loop");
      auto after_block =
          fback.create_block(LexScope::LOOP_AFTER_BLOCK, "after");
      auto merge_block = fback.create_block(LexScope::MERGE_BLOCK, "merge");
      fback.metadata.add(LexScope::LOOP_BREAK_LABEL, merge_block);
      fback.metadata.add(LexScope::LOOP_CONTINUE_LABEL, after_block);
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      fback.push_scope(true);
      auto loop_block = fback.create_block(LexScope::LOOP_BODY_BLOCK, "loop");
      auto cond_block = fback.create_block(LexScope::LOOP_COND_BLOCK, "cond");
      auto merge_block = fback.create_block(LexScope::MERGE_BLOCK, "merge");
      fback.metadata.add(LexScope::LOOP_BREAK_LABEL, merge_block);
      fback.metadata.add(LexScope::LOOP_CONTINUE_LABEL, cond_block);
      fback.push_scope(true);

      _b.b.CreateBr(loop_block);
      _b.b.SetInsertPoint(loop_block);
      break;
    }

    default: {}
  }
}

void IrGenerator::infix(const Node& node, const result_list& results)
{
  auto& fback = _scopes.back();
  switch (node.type) {
    case Node::FUNCTION:
    {
      fback.metadata.pop();
      create_function(node, results[0].type);
      break;
    }

    case Node::IF_STMT:
    {
      auto then_block = fback.get_block(LexScope::IF_THEN_BLOCK);
      auto else_block = fback.get_block(LexScope::IF_ELSE_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);

      if (results.size() == 1) {
        bool has_else = node.children.size() > 2;
        _b.b.CreateCondBr(i2b(results[0]), then_block,
                          has_else ? else_block : merge_block);
        _b.b.SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        _b.b.CreateBr(merge_block);
        _b.b.SetInsertPoint(else_block);
      }
      break;
    }

    case Node::FOR_STMT:
    {
      auto cond_block = fback.get_block(LexScope::LOOP_COND_BLOCK);
      auto loop_block = fback.get_block(LexScope::LOOP_BODY_BLOCK);
      auto after_block = fback.get_block(LexScope::LOOP_AFTER_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);

      if (results.size() == 1) {
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(cond_block);
        fback.push_scope(true);
      }
      if (results.size() == 2) {
        auto parent = _b.b.GetInsertBlock()->getParent();
        auto clean_block =
            llvm::BasicBlock::Create(_b.b.getContext(), "clean", parent);
        _b.b.CreateCondBr(i2b(results[1]), loop_block, clean_block);
        _b.b.SetInsertPoint(clean_block);
        fback.dereference_scoped_locals();
        _b.b.CreateBr(merge_block);
        _b.b.SetInsertPoint(after_block);
      }
      if (results.size() == 3) {
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(loop_block);
      }
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      auto cond_block = fback.get_block(LexScope::LOOP_COND_BLOCK);
      fback.pop_scope(true);
      fback.push_scope();
      _b.b.CreateBr(cond_block);
      _b.b.SetInsertPoint(cond_block);
      break;
    }

    case Node::TERNARY:
    {
      // Vectorised ternary can't short-circuit.
      if (results[0].type.is_vector()) {
        break;
      }

      if (results.size() == 1) {
        fback.metadata.push();

        // Blocks and branching are necessary (as opposed to a select
        // instruction) to short-circuit and avoiding evaluating the other path.
        auto then_block = fback.create_block(LexScope::IF_THEN_BLOCK, "then");
        auto else_block = fback.create_block(LexScope::IF_ELSE_BLOCK, "else");
        fback.create_block(LexScope::MERGE_BLOCK, "merge");

        _b.b.CreateCondBr(i2b(results[0]), then_block, else_block);
        _b.b.SetInsertPoint(then_block);
        fback.push_scope();
      }
      if (results.size() == 2) {
        fback.pop_scope();
        auto else_block = fback.get_block(LexScope::IF_ELSE_BLOCK);
        auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);
        _b.b.CreateBr(merge_block);

        // Metadata blocks must always be updated to the current one, in case we
        // created a bunch of new ones before we got to the end!
        fback.metadata[LexScope::IF_THEN_BLOCK] = _b.b.GetInsertBlock();
        _b.b.SetInsertPoint(else_block);
        fback.push_scope();
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
      if (results[0].type.is_vector()) {
        break;
      }
      fback.metadata.push();
      fback.metadata.add(LexScope::LOGICAL_OP_SOURCE_BLOCK,
                         &*_b.b.GetInsertPoint());
      auto rhs_block =
          fback.create_block(LexScope::LOGICAL_OP_RHS_BLOCK, "rhs");
      auto merge_block = fback.create_block(LexScope::MERGE_BLOCK, "merge");

      if (node.type == Node::LOGICAL_OR) {
        _b.b.CreateCondBr(i2b(results[0]), merge_block, rhs_block);
      }
      else {
        _b.b.CreateCondBr(i2b(results[0]), rhs_block, merge_block);
      }
      _b.b.SetInsertPoint(rhs_block);
      fback.push_scope();
      break;
    }

    default: {}
  }
}

Value IrGenerator::visit(const Node& node, const result_list& results)
{
  auto parent = _b.b.GetInsertBlock() ?
      _b.b.GetInsertBlock()->getParent() : nullptr;
  auto& fback = _scopes.back();

  switch (node.type) {
    case Node::TYPE_VOID:
      return yang::Type::void_t();
    case Node::TYPE_INT:
      return node.int_value > 1 ?
          yang::Type::int_vector_t(node.int_value) : yang::Type::int_t();
    case Node::TYPE_FLOAT:
      return node.int_value > 1 ?
          yang::Type::float_vector_t(node.int_value) : yang::Type::float_t();
    case Node::TYPE_FUNCTION:
    {
      type_function:
      std::vector<yang::Type> args;
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i].type);
      }
      return yang::Type::function_t(results[0].type, args);
    }

    case Node::GLOBAL:
      fback.dereference_scoped_locals(0);
      // TODO: can we stick this in refcount_init instead somehow?
      if (fback.metadata[LexScope::CLOSURE_PTR]) {
        fback.update_reference_count(
            nullptr, fback.metadata[LexScope::CLOSURE_PTR], -1);
      }
      _scopes.pop_back();
      _b.b.CreateRetVoid();
      return Value();
    case Node::GLOBAL_ASSIGN:
    {
      auto function = (llvm::Function*)parent;
      get_trampoline_function(results[1].type, false);
      // Top-level functions Nodes have their int_value set to 1 when defined
      // using the `export` keyword.
      if (node.int_value) {
        function->setLinkage(llvm::Function::ExternalLinkage);
      }
      function->setName(node.children[0]->string_value);
      fback.symbol_table.add(node.children[0]->string_value, results[1]);
      return results[1];
    }
    case Node::FUNCTION:
    {
      if (results[0].type.get_function_return_type().is_void()) {
        fback.dereference_scoped_locals(0);
        if (fback.metadata[LexScope::CLOSURE_PTR]) {
          fback.update_reference_count(
              nullptr, fback.metadata[LexScope::CLOSURE_PTR], -1);
        }
        _b.b.CreateRetVoid();
      }
      else {
        // In a function that passes the static check, control never reaches
        // this point; but the block must have a terminator.
        _b.b.CreateBr(_b.b.GetInsertBlock());
      }
      auto parent_block = fback.get_block(LexScope::PARENT_BLOCK);
      _scopes.pop_back();

      auto& fprev = _scopes.back();
      if (!fprev.metadata.has(LexScope::FUNCTION)) {
        return Value(results[0].type, parent);
      }
      // If this was a nested function, set the insert point back to the last
      // block in the enclosing function and make the proper expression value.
      _b.b.SetInsertPoint(parent_block);
      // If the current function created a closure, we have to pass the new
      // environment pointer (which has a parent pointer to the old one)
      // instead. There is some potential for optimisation if an entire tree
      // of inner functions never creates a closure, where we could instead
      // dereference and pass only the closure scopes they actually access; it's
      // kind of complicated and not really a huge optimisation, though.
      auto closure = fprev.metadata[LexScope::CLOSURE_PTR];
      return _b.function_value(
          results[0].type, parent,
          closure ? closure : fprev.metadata[LexScope::ENVIRONMENT_PTR]);
    }
    case Node::NAMED_EXPRESSION:
      return results[0];
      break;

    case Node::BLOCK:
    {
      auto after_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "after", parent);
      _b.b.CreateBr(after_block);
      _b.b.SetInsertPoint(after_block);
      fback.pop_scope();
      return Value();
    }
    case Node::EMPTY_STMT:
      return _b.constant_int(0);
    case Node::EXPR_STMT:
      return results[0];
    case Node::RETURN_VOID_STMT:
    case Node::RETURN_STMT:
    {
      auto dead_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "dead", parent);
      fback.dereference_scoped_locals(0);
      if (fback.metadata[LexScope::CLOSURE_PTR]) {
        fback.update_reference_count(
            nullptr, fback.metadata[LexScope::CLOSURE_PTR], -1);
      }
      node.type == Node::RETURN_STMT ?
          _b.b.CreateRet(results[0]) : _b.b.CreateRetVoid();
      _b.b.SetInsertPoint(dead_block);
      return node.type == Node::RETURN_STMT ? results[0] : Value();
    }
    case Node::IF_STMT:
    {
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);

      _b.b.CreateBr(merge_block);
      _b.b.SetInsertPoint(merge_block);
      fback.pop_scope();
      return results[0];
    }
    case Node::FOR_STMT:
    {
      auto after_block = fback.get_block(LexScope::LOOP_AFTER_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);
      fback.pop_scope(true);

      _b.b.CreateBr(after_block);
      _b.b.SetInsertPoint(merge_block);
      fback.pop_scope();
      return results[0];
    }
    case Node::DO_WHILE_STMT:
    {
      auto loop_block = fback.get_block(LexScope::LOOP_BODY_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);
      fback.pop_scope();
      fback.pop_scope();

      _b.b.CreateCondBr(i2b(results[1]), loop_block, merge_block);
      _b.b.SetInsertPoint(merge_block);
      return results[0];
    }
    case Node::BREAK_STMT:
    case Node::CONTINUE_STMT:
    {
      fback.dereference_loop_locals();
      auto dead_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "dead", parent);
      _b.b.CreateBr(fback.get_block(
          node.type == Node::BREAK_STMT ? LexScope::LOOP_BREAK_LABEL :
                                          LexScope::LOOP_CONTINUE_LABEL));
      _b.b.SetInsertPoint(dead_block);
      return Value();
    }

    case Node::SCOPE_RESOLUTION:
    {
      // Scope resolution always refers to a context function.
      std::string s =
          node.static_info.user_type_name + "::" + node.string_value;
      auto it = _context.get_functions().find(s);
      return _b.function_value(it->second);
    }
    case Node::MEMBER_SELECTION:
    {
      std::string s =
          node.static_info.user_type_name + "::" + node.string_value;
      llvm::Value* env = _member_function_closure.allocate_closure_struct(
          _b.constant_ptr(nullptr));
      std::size_t index =
          _member_function_closure.structure.table["object"].index;
      _member_function_closure.memory_store(
          results[0], _member_function_closure.structure_ptr(env, index));
      Value v = get_member_function(s);
      v = _b.function_value(v.type, v, env);
      fback.update_reference_count(v, 1);
      fback.refcount_init(v);
      return v;
    }

    case Node::IDENTIFIER:
    {
      // In type-context, we just want to return a user type.
      if (fback.metadata.has(LexScope::TYPE_EXPR_CONTEXT)) {
        return yang::Type::user_t();
      }

      // Load the variable, if it's there. We must be careful to only return
      // globals/functions *after* they have been defined, otherwise it might
      // be a reference to a context function of the same name.
      Value variable_ptr = get_variable_ptr(node.string_value);
      if (variable_ptr) {
        return variable_ptr.irval->getType()->isPointerTy() ?
            fback.memory_load(variable_ptr.type, variable_ptr) : variable_ptr;
      }

      // It's possible that nothing matches, when this is the identifier on the
      // left of a variable declaration.
      auto it = _context.get_functions().find(node.string_value);
      if (it == _context.get_functions().end()) {
        return Value();
      }
      // It must be a context function.
      return _b.function_value(it->second);
    }

    case Node::INT_LITERAL:
      return _b.constant_int(node.int_value);
    case Node::FLOAT_LITERAL:
      return _b.constant_float(node.float_value);

    case Node::TERNARY:
    {
      if (results[0].type.is_vector()) {
        // Vectorised ternary. Short-circuiting isn't possible.
        return Value(
            results[1].type,
            _b.b.CreateSelect(i2b(results[0]), results[1], results[2]));
      }
      fback.pop_scope();
      // Update in case we branched again.
      fback.metadata[LexScope::IF_ELSE_BLOCK] = _b.b.GetInsertBlock();

      auto then_block = fback.get_block(LexScope::IF_THEN_BLOCK);
      auto else_block = fback.get_block(LexScope::IF_ELSE_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);
      fback.metadata.pop();
      _b.b.CreateBr(merge_block);
      _b.b.SetInsertPoint(merge_block);
      auto phi = _b.b.CreatePHI(_b.get_llvm_type(results[1].type), 2);
      phi->addIncoming(results[1], then_block);
      phi->addIncoming(results[2], else_block);
      return Value(results[1].type, phi);
    }
    case Node::CALL:
    {
      if (fback.metadata.has(LexScope::TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }
      std::vector<Value> args;
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i]);
      }
      Value r = create_call(results[0], args);
      // Reference count the temporary.
      if (r) {
        fback.update_reference_count(r, 1);
        fback.refcount_init(r);
      }
      return r;
    }

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    {
      if (results[0].type.is_vector()) {
        // Short-circuiting isn't possible.
        return binary(node, b2i(i2b(results[0])), b2i(i2b(results[1])));
      }
      fback.pop_scope();
      // Update in case we branched again.
      fback.metadata[LexScope::LOGICAL_OP_RHS_BLOCK] = _b.b.GetInsertBlock();

      auto source_block = fback.get_block(LexScope::LOGICAL_OP_SOURCE_BLOCK);
      auto rhs_block = fback.get_block(LexScope::LOGICAL_OP_RHS_BLOCK);
      auto merge_block = fback.get_block(LexScope::MERGE_BLOCK);
      fback.metadata.pop();

      auto rhs = b2i(i2b(results[1]));
      _b.b.CreateBr(merge_block);
      _b.b.SetInsertPoint(merge_block);
      llvm::Type* type = _b.int_type();
      llvm::Value* constant = nullptr;
      if (results[1].type.is_vector()) {
        std::size_t n = results[1].type.get_vector_size();
        constant =
            _b.constant_int_vector(node.type == Node::LOGICAL_OR, n);
        type = _b.int_vector_type(n);
      }
      else {
        constant = _b.constant_int(node.type == Node::LOGICAL_OR);
      }

      auto phi = _b.b.CreatePHI(type, 2);
      phi->addIncoming(constant, source_block);
      phi->addIncoming(rhs, rhs_block);
      return Value(results[1].type, phi);
    }

    // Most binary operators map directly to (vectorisations of) LLVM IR
    // instructions.
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
      return binary(node, results[0], results[1]);

    case Node::POW:
    case Node::MOD:
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
      return binary(node, results[0], results[1]);
    case Node::EQ:
    case Node::NE:
    case Node::GE:
    case Node::LE:
    case Node::GT:
    case Node::LT:
      return b2i(binary(node, results[0], results[1]));

    case Node::FOLD_LOGICAL_OR:
    case Node::FOLD_LOGICAL_AND:
      return b2i(fold(node, results[0], true));

    case Node::FOLD_BITWISE_OR:
    case Node::FOLD_BITWISE_AND:
    case Node::FOLD_BITWISE_XOR:
    case Node::FOLD_BITWISE_LSHIFT:
    case Node::FOLD_BITWISE_RSHIFT:
      return fold(node, results[0]);

    case Node::FOLD_POW:
      // POW is the only right-associative fold operator.
      return fold(node, results[0], false, false, true);
    case Node::FOLD_MOD:
    case Node::FOLD_ADD:
    case Node::FOLD_SUB:
    case Node::FOLD_MUL:
    case Node::FOLD_DIV:
      return fold(node, results[0]);

    case Node::FOLD_EQ:
    case Node::FOLD_NE:
    case Node::FOLD_GE:
    case Node::FOLD_LE:
    case Node::FOLD_GT:
    case Node::FOLD_LT:
      return b2i(fold(node, results[0], false, true));

    case Node::LOGICAL_NEGATION:
    {
      Value v = _b.default_for_type(results[0].type);
      v.irval = _b.b.CreateICmpEQ(results[0], v);
      return b2i(v);
    }
    case Node::BITWISE_NEGATION:
    {
      Value v = _b.default_for_type(results[0].type, 0u - 1);
      v.irval = _b.b.CreateXor(results[0], v);
      return v;
    }
    case Node::ARITHMETIC_NEGATION:
    {
      Value v = _b.default_for_type(results[0].type);
      v.irval = results[0].type.is_int() || results[0].type.is_int_vector() ?
          _b.b.CreateSub(v, results[0]) : _b.b.CreateFSub(v, results[0]);
      return v;
    }
    case Node::INCREMENT:
    case Node::DECREMENT:
    {
      Value v = _b.default_for_type(
          results[0].type, node.type == Node::INCREMENT ? 1 : -1);
      v.irval = results[0].type.is_int() || results[0].type.is_int_vector() ?
          _b.b.CreateAdd(results[0], v) : _b.b.CreateFAdd(results[0], v);
      if (node.children[0]->type == Node::IDENTIFIER) {
        const std::string& s = node.children[0]->string_value;
        fback.memory_store(v, get_variable_ptr(s));
      }
      return v;
    }

    case Node::ASSIGN:
    {
      const std::string& s = node.children[0]->string_value;
      fback.memory_store(results[1], get_variable_ptr(s));
      return results[1];
    }

    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
    {
      const std::string& s = node.children[0]->string_value;
      // In a global block, rather than allocating anything we simply store into
      // the prepared fields of the global structure. Also enter the symbol now
      // with a null value so we can distinguish it from top-level functions.
      if (fback.metadata.has(LexScope::GLOBAL_INIT_FUNCTION) &&
          fback.symbol_table.size() <= 2) {
        _scopes[0].symbol_table.add(s, Value(results[1].type));
        _scopes[0].memory_store(results[1], get_variable_ptr(s));
        return results[1];
      }

      const std::string& unique_name =
          s + "/" + std::to_string(node.static_info.scope_number);

      llvm::Value* storage = nullptr;
      if (fback.symbol_table.has(unique_name)) {
        storage = fback.symbol_table[unique_name];
        fback.value_to_unique_name_map.emplace(storage, unique_name);
      }
      else {
        // Optimisation passes such as mem2reg work much better when memory
        // locations are declared in the entry block (so they are guaranteed to
        // execute once).
        auto llvm_function =
            (llvm::Function*)_b.b.GetInsertPoint()->getParent();
        auto& entry_block = llvm_function->getEntryBlock();
        llvm::IRBuilder<> entry(&entry_block, entry_block.begin());
        storage =
            entry.CreateAlloca(_b.get_llvm_type(results[1].type), nullptr);
        fback.memory_init(entry, storage);
        fback.refcount_init(Value(results[1].type, storage));
      }

      fback.memory_store(results[1], storage);
      fback.symbol_table.add(s, Value(results[1].type, storage));
      return results[1];
    }

    case Node::INT_CAST:
      return f2i(results[0]);
    case Node::FLOAT_CAST:
      return i2f(results[0]);

    case Node::VECTOR_CONSTRUCT:
    {
      Value v = results[0].type.is_int() ?
          _b.constant_int_vector(0, results.size()) :
          _b.constant_float_vector(0, results.size());
      for (std::size_t i = 0; i < results.size(); ++i) {
        v.irval = _b.b.CreateInsertElement(
            v.irval, results[i], _b.constant_int(i));
      }
      return v;
    }
    case Node::VECTOR_INDEX:
    {
      // Indexing out-of-bounds produces constant zero.
      Value v = results[0].type.is_int_vector() ?
          _b.constant_int(0) : _b.constant_float(0);
      auto ge = _b.b.CreateICmpSGE(results[1], _b.constant_int(0));
      auto lt = _b.b.CreateICmpSLT(
          results[1], _b.constant_int(results[0].type.get_vector_size()));

      auto in = _b.b.CreateAnd(ge, lt);
      v.irval =_b.b.CreateSelect(
          in, _b.b.CreateExtractElement(results[0], results[1]), v);
      return v;
    }

    default:
      return Value();
  }
}

llvm::Value* IrGenerator::get_parent_struct(
    std::size_t parent_steps, llvm::Value* v)
{
  llvm::Type* void_ptr_ptr = llvm::PointerType::get(
      llvm::StructType::create(_b.b.getContext(), _b.void_ptr_type()), 0);
  llvm::Value* u = v;
  for (std::size_t i = 0; i < parent_steps; ++i) {
    u = _scopes.back().memory_load(
        yang::Type::void_t(),
        _scopes.back().structure_ptr(_b.b.CreateBitCast(u, void_ptr_ptr), 0));
  }
  return u;
}

Value IrGenerator::get_variable_ptr(const std::string& name)
{
  std::size_t steps = 0;
  auto it = _scopes.rbegin();
  // Local variables.
  if (it->symbol_table.has(name)) {
    return it->symbol_table[name];
  }
  // Nonlocal variables.
  for (++it; ; ++it) {
    if (it == _scopes.rend()) {
      return Value();
    }
    if (it->symbol_table.has(name)) {
      break;
    }
    if (it->structure.type) {
      ++steps;
    }
  }

  std::string unique_name = name;
  llvm::Value* struct_ptr = get_parent_struct(
      steps, _scopes.back().metadata[LexScope::ENVIRONMENT_PTR]);
  llvm::Value* cast = _b.b.CreateBitCast(struct_ptr, it->structure.type);

  if (it == --_scopes.rend()) {
    // Global variables.
    // If the symbol table entry is non-null it's a top-level function and has
    // to be handled separately.
    if (it->symbol_table[name]) {
      const auto& sym = it->symbol_table[name];
      return _b.function_value(sym.type, sym, cast);
    }
  }
  else {
    // To look up a value in a closure, the flow is:
    // [std::string identifier] through _symbol_table to
    // [llvm::Value* value (in defining function)] through
    // _value_to_unique_name_map to [std::string unique_identifier].
    unique_name = it->value_to_unique_name_map[it->symbol_table[name]];
  }

  return Value(
      it->symbol_table[name].type,
      it->structure_ptr(cast, it->structure.table[unique_name].index));
}

void IrGenerator::create_function(
    const Node& node, const yang::Type& function_type)
{
  // Linkage will be set later if necessary.
  auto llvm_type = _b.raw_function_type(function_type);
  auto function = llvm::Function::Create(
      llvm_type, llvm::Function::InternalLinkage, "anonymous", &_b.module);

  auto eptr = --function->arg_end();
  eptr->setName("env");
  _scopes.emplace_back(_scopes.back().next_lex_scope());
  auto& fback = _scopes.back();

  // The code for Node::TYPE_FUNCTION in visit() ensures it takes an environment
  // pointer.
  fback.metadata.add(LexScope::ENVIRONMENT_PTR, &*eptr);
  fback.metadata.add(LexScope::FUNCTION, function);
  fback.metadata.add(LexScope::PARENT_BLOCK, &*_b.b.GetInsertPoint());

  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);
  _b.b.SetInsertPoint(block);

  // Some optimisations may be possible. For example, const variables may not
  // need to go in the closure structure. Maybe LLVM can handle that anyway?
  //
  // Also, we naively allocate at most one closure structure per function
  // invocation. Obviously, if we could partition the inner functions such that
  // the sets of enclosing variables they access are disjoint, we could allocate
  // separate structures for each (and potentially return unused memory sooner).i
  if (!node.static_info.closed_environment.empty()) {
    fback.init_structure_type(node.static_info.closed_environment, false);
    llvm::Value* v = fback.allocate_closure_struct(&*eptr);
    fback.metadata[LexScope::CLOSURE_PTR] = v;
    // Refcounting on closure pointer.
    fback.update_reference_count(nullptr, v, 1);
  }

  // Lambda for deciding whether to put an argument in closure or stack. These
  // are refcount-decremented on return statements (or end-of-function for void
  // returns).
  auto assign_storage = [&](
      const yang::Type& type, const std::string& name, std::int32_t scope_mod)
  {
    std::string unique_name =
        name + "/" + std::to_string(node.static_info.scope_number + scope_mod);

    if (fback.symbol_table.has(unique_name)) {
      llvm::Value* v = fback.symbol_table[unique_name];
      fback.value_to_unique_name_map.emplace(v, unique_name);
      return v;
    }
    // Rather than reference argument values directly, we create an alloca
    // and store the argument in there. This simplifies things, since we
    // can emit the same IR code when referencing local variables or
    // function arguments.
    llvm::Value* v = _b.b.CreateAlloca(_b.get_llvm_type(type), nullptr);
    fback.refcount_init(Value(type, v));
    fback.memory_init(_b.b, v);
    return v;
  };

  // Store the function's name in its own scope. Recursive lookup handled
  // similarly to arguments below.
  if (_immediate_left_assign.length()) {
    function->setName(_immediate_left_assign);
    // The identifier is registered one scope above the function argument scope.
    llvm::Value* storage =
        assign_storage(function_type, _immediate_left_assign, -1);
    fback.memory_store(
        _b.function_value(function_type, function, eptr), storage);
    fback.symbol_table.add(
        _immediate_left_assign, Value(function_type, storage));
    _immediate_left_assign.clear();
  }
  fback.symbol_table.push();

  // Set up the arguments.
  std::size_t arg_num = 0;
  for (auto it = function->arg_begin();
       it != --function->arg_end(); ++it, ++arg_num) {
    const std::string& name =
        node.children[0]->children[1 + arg_num]->string_value;
    const yang::Type& arg = function_type.get_function_arg_type(arg_num);
    llvm::Value* storage = assign_storage(arg, name, 0);
    it->setName(name);

    // It's possible refcounting isn't necessary on arguments, since they're
    // const and will usually be referenced somewhere up the call stack. I'm not
    // convinced, though.
    fback.memory_store(Value(arg, &*it), storage);
    fback.symbol_table.add(name, Value(arg, storage));
  }

  // Inform the optimiser that the eptr will never be null. This allows a lot
  // of simplification in most cases.
  // TODO: LLVM doesn't make as much use of it as I'd like, though, for reasons
  // I can't quite work out. It correctly optimises out the C++ function check
  // for recursive calls, and similarly for some refcount calls, but somehow
  // it can't optimise out the very first check for the eptr refcounting?
  auto notnull_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "notnull", function);
  auto main_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "main", function);
  llvm::Value* cmp = _b.b.CreateIsNull(eptr);
  _b.b.CreateCondBr(cmp, notnull_block, main_block);
  _b.b.SetInsertPoint(notnull_block);
  _b.b.CreateUnreachable();
  _b.b.SetInsertPoint(main_block);
}

Value IrGenerator::get_member_function(const std::string& name)
{
  auto it = _member_functions.find(name);
  if (it != _member_functions.end()) {
    return it->second;
  }

  auto jt = _context.get_functions().find(name);
  const yang::Type& full_type = jt->second.type;
  std::vector<yang::Type> args;
  for (std::size_t i = 1; i < full_type.get_function_num_args(); ++i) {
    args.push_back(full_type.get_function_arg_type(i));
  }
  yang::Type closed_type =
      yang::Type::function_t(full_type.get_function_return_type(), args);

  auto llvm_type = _b.raw_function_type(closed_type);
  auto f = llvm::Function::Create(
      llvm_type, llvm::Function::InternalLinkage, name, &_b.module);

  llvm::Value* closure = --f->arg_end();
  closure->setName("boxed_object");
  llvm::BasicBlock* prev = _b.b.GetInsertBlock();
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", f);
  _b.b.SetInsertPoint(block);

  Value genf = _b.function_value(jt->second);
  closure = _b.b.CreateBitCast(
      closure, _member_function_closure.structure.type);
  std::size_t index = _member_function_closure.structure.table["object"].index;
  auto object = _member_function_closure.memory_load(
      yang::Type::void_t(),
      _member_function_closure.structure_ptr(closure, index));

  std::vector<Value> fargs;
  fargs.emplace_back(full_type.get_function_arg_type(0), object);
  std::size_t i = 0;
  for (auto it = f->arg_begin(); it != --f->arg_end(); ++it) {
    fargs.emplace_back(full_type.get_function_arg_type(++i), it);
  }
  _b.b.CreateRet(create_call(genf, fargs));
  _b.b.SetInsertPoint(prev);

  Value v(closed_type, f);
  _member_functions.emplace(name, v);
  return v;
}

Value IrGenerator::create_call(const Value& f, const std::vector<Value>& args)
{
  std::vector<llvm::Value*> llvm_args;
  for (const auto& a : args) {
    llvm_args.push_back(a);
  }

  // Extract pointers from the struct.
  llvm::Value* fptr = _b.b.CreateExtractValue(f, 0);
  llvm::Value* eptr = _b.b.CreateExtractValue(f, 1);
  llvm_args.push_back(eptr);

  // Switch on the presence of a trampoline function pointer.
  auto parent = _b.b.GetInsertBlock()->getParent();
  auto cpp_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "cpp", parent);
  auto yang_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "yang", parent);
  auto merge_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "merge", parent);

  llvm::Value* cmp = _b.b.CreateIsNull(eptr);
  _b.b.CreateCondBr(cmp, cpp_block, yang_block);

  _b.b.SetInsertPoint(yang_block);
  llvm::Value* cast = _b.b.CreateBitCast(
      fptr, llvm::PointerType::get(_b.raw_function_type(f.type), 0));
  llvm::Value* yang_val = _b.b.CreateCall(cast, llvm_args);
  _b.b.CreateBr(merge_block);

  // Don't bother to generate correct code when the C++ path can never be
  // taken (because that trampoline type has never been generated).
  llvm::Value* trampoline = get_reverse_trampoline_function(f.type, false);
  llvm::Value* cpp_val = nullptr;
  _b.b.SetInsertPoint(cpp_block);
  if (trampoline) {
    llvm_args.push_back(_b.b.CreateBitCast(fptr, _b.void_ptr_type()));
    cpp_val = _b.b.CreateCall(trampoline, llvm_args);
    _b.b.CreateBr(merge_block);
  }
  else {
    _b.b.CreateBr(yang_block);
  }

  _b.b.SetInsertPoint(merge_block);
  const yang::Type& return_t = f.type.get_function_return_type();
  if (!return_t.is_void()) {
    auto phi_v = _b.b.CreatePHI(_b.get_llvm_type(return_t), 2);
    Value phi(return_t, phi_v);

    if (trampoline) {
      phi_v->addIncoming(cpp_val, cpp_block);
    }
    phi_v->addIncoming(yang_val, yang_block);
    return phi;
  }
  return Value();
}

Value IrGenerator::i2b(const Value& v)
{
  Value result = _b.default_for_type(v.type);
  result.irval = _b.b.CreateICmpNE(v, result);
  return result;
}

Value IrGenerator::b2i(const Value& v)
{
  if (v.type.is_vector()) {
    std::size_t size = v.type.get_vector_size();
    return Value(yang::Type::int_vector_t(size),
                 _b.b.CreateZExt(v, _b.int_vector_type(size)));
  }
  return Value(yang::Type::int_t(), _b.b.CreateZExt(v, _b.int_type()));
}

Value IrGenerator::i2f(const Value& v)
{
  if (v.type.is_vector()) {
    std::size_t size = v.type.get_vector_size();
    return Value(yang::Type::float_vector_t(size),
                 _b.b.CreateSIToFP(v, _b.float_vector_type(size)));
  }
  return Value(yang::Type::float_t(), _b.b.CreateSIToFP(v, _b.float_type()));
}

Value IrGenerator::f2i(const Value& v)
{
  yang::Type type = v.type.is_vector() ?
      yang::Type::int_vector_t(v.type.get_vector_size()) : yang::Type::int_t();
  Value zero = _b.default_for_type(v.type);

  // Mathematical floor. Implements the algorithm:
  // return int(v) - (v < 0 && v != float(int(v)));
  auto cast = _b.b.CreateFPToSI(v, _b.get_llvm_type(type));
  auto back = _b.b.CreateSIToFP(cast, _b.get_llvm_type(v.type));

  auto a_check = _b.b.CreateFCmpOLT(v, zero);
  auto b_check = _b.b.CreateFCmpONE(v, back);
  auto and_v = b2i(Value(type, _b.b.CreateAnd(a_check, b_check)));
  return Value(type, _b.b.CreateSub(cast, and_v));
}

Value IrGenerator::raw_binary(const Node& node, const Value& v, const Value& u)
{
  llvm::Value* vi = v;
  llvm::Value* ui = u;

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

  if (v.type.is_int() or v.type.is_int_vector()) {
    llvm::Value* out =
        type == Node::LOGICAL_OR ? _b.b.CreateOr(vi, ui) :
        type == Node::LOGICAL_AND ? _b.b.CreateAnd(vi, ui) :
        type == Node::BITWISE_OR ? _b.b.CreateOr(vi, ui) :
        type == Node::BITWISE_AND ? _b.b.CreateAnd(vi, ui) :
        type == Node::BITWISE_XOR ? _b.b.CreateXor(vi, ui) :
        type == Node::BITWISE_LSHIFT ? lsh(v, u) :
        type == Node::BITWISE_RSHIFT ? rsh(v, u) :
        type == Node::POW ? pow(v, u) :
        type == Node::MOD ? mod(v, u) :
        type == Node::ADD ? _b.b.CreateAdd(vi, ui) :
        type == Node::SUB ? _b.b.CreateSub(vi, ui) :
        type == Node::MUL ? _b.b.CreateMul(vi, ui) :
        type == Node::DIV ? div(v, u) :
        type == Node::EQ ? _b.b.CreateICmpEQ(vi, ui) :
        type == Node::NE ? _b.b.CreateICmpNE(vi, ui) :
        type == Node::GE ? _b.b.CreateICmpSGE(vi, ui) :
        type == Node::LE ? _b.b.CreateICmpSLE(vi, ui) :
        type == Node::GT ? _b.b.CreateICmpSGT(vi, ui) :
        type == Node::LT ? _b.b.CreateICmpSLT(vi, ui) :
        nullptr;
    return Value(v.type, out);
  }

  llvm::Value* out =
      type == Node::POW ? pow(v, u) :
      type == Node::MOD ? mod(v, u) :
      type == Node::ADD ? _b.b.CreateFAdd(vi, ui) :
      type == Node::SUB ? _b.b.CreateFSub(vi, ui) :
      type == Node::MUL ? _b.b.CreateFMul(vi, ui) :
      type == Node::DIV ? div(v, u) :
      type == Node::EQ ? _b.b.CreateFCmpOEQ(vi, ui) :
      type == Node::NE ? _b.b.CreateFCmpONE(vi, ui) :
      type == Node::GE ? _b.b.CreateFCmpOGE(vi, ui) :
      type == Node::LE ? _b.b.CreateFCmpOLE(vi, ui) :
      type == Node::GT ? _b.b.CreateFCmpOGT(vi, ui) :
      type == Node::LT ? _b.b.CreateFCmpOLT(vi, ui) :
      nullptr;
  return Value(v.type, out);
}

llvm::Value* IrGenerator::vectorise(
    const Value& v, const Value& u, llvm::Function* f, bool to_float)
{
  auto flt = [&](llvm::Value* t)
  {
    Value tt(yang::Type::int_t(), t);
    return to_float ? i2f(tt) : tt;
  };
  if (!v.type.is_vector()) {
    llvm::Value* r =
        _b.b.CreateCall(f, std::vector<llvm::Value*>{flt(v), flt(u)});
    return to_float && v.type.is_int() ?
        f2i(Value(yang::Type::float_t(), r)) : r;
  }

  std::size_t size = v.type.get_vector_size();
  Value result = to_float ?
      _b.constant_float_vector(0, size) : _b.default_for_type(v.type);
  for (std::size_t i = 0; i < size; ++i) {
    std::vector<llvm::Value*> args{
        flt(_b.b.CreateExtractElement(v, _b.constant_int(i))),
        flt(_b.b.CreateExtractElement(u, _b.constant_int(i)))};
    llvm::Value* call = _b.b.CreateCall(f, args);
    result.irval = _b.b.CreateInsertElement(result, call, _b.constant_int(i));
  }
  return to_float && v.type.is_int_vector() ? f2i(result) : result;
}

llvm::Value* IrGenerator::lsh(const Value& v, const Value& u)
{
  std::vector<llvm::Type*> args{_b.int_type(), _b.int_type()};
  auto lsh_ptr = _b.get_native_function(
      "lsh", (yang::void_fp)&::lsh,
      llvm::FunctionType::get(_b.int_type(), args, false));
  return vectorise(v, u, lsh_ptr);
}

llvm::Value* IrGenerator::rsh(const Value& v, const Value& u)
{
  std::vector<llvm::Type*> args{_b.int_type(), _b.int_type()};
  auto rsh_ptr = _b.get_native_function(
      "rsh", (yang::void_fp)&::rsh,
      llvm::FunctionType::get(_b.int_type(), args, false));
  return vectorise(v, u, rsh_ptr);
}

llvm::Value* IrGenerator::pow(const Value& v, const Value& u)
{
  std::vector<llvm::Type*> args{_b.float_type(), _b.float_type()};
  auto pow_ptr = _b.get_native_function(
      "pow", (yang::void_fp)&::pow,
      llvm::FunctionType::get(_b.float_type(), args, false));
  return vectorise(v, u, pow_ptr, true);
}

llvm::Value* IrGenerator::mod(const Value& v, const Value& u)
{
  // Should be able to use LLVM "frem" instruction, but breaks on e.g.
  // const t = 1; return t. % 2.;
  if (!v.type.is_int() && !v.type.is_int_vector()) {
    std::vector<llvm::Type*> args{_b.float_type(), _b.float_type()};
    auto fmod_ptr = _b.get_native_function(
        "fmod", (yang::void_fp)&::fmod,
        llvm::FunctionType::get(_b.float_type(), args, false));
    return vectorise(v, u, fmod_ptr);
  }

  Value zero = _b.default_for_type(v.type);
  // Implements the following algorithm:
  // return (v >= 0 ? v : v + (bool(|v| % |u|) + |v| / |u|) * |u|) % |u|;
  // There are simpler ways, but they are vulnerable to overflow errors.
  // k = |v| % |u| + |v| / |u| is the smallest postive integer such that
  // k * |u| >= |v|.
  auto v_check = _b.b.CreateICmpSGE(v, zero);
  auto u_check = _b.b.CreateICmpSGE(u, zero);
  auto v_abs = _b.b.CreateSelect(v_check, v, _b.b.CreateSub(zero, v));
  auto u_abs = _b.b.CreateSelect(u_check, u, _b.b.CreateSub(zero, u));

  Value rem(v.type, _b.b.CreateSRem(v_abs, u_abs));
  auto k = _b.b.CreateAdd(b2i(i2b(rem)), _b.b.CreateSDiv(v_abs, u_abs));
  auto lhs = _b.b.CreateSelect(
      v_check, v, _b.b.CreateAdd(v, _b.b.CreateMul(k, u_abs)));
  return _b.b.CreateSRem(lhs, u_abs);
}

llvm::Value* IrGenerator::div(const Value& v, const Value& u)
{
  if (!v.type.is_int() && !v.type.is_int_vector()) {
    return _b.b.CreateFDiv(v, u);
  }

  Value zero = _b.default_for_type(v.type);
  Value minus_one = _b.default_for_type(v.type, -1);
  // Implements the following algorithm:
  // bool sign = (v < 0) == (u < 0);
  // int t = (v < 0 ? -(1 + v) : v) / |u|;
  // return (sign ? t : -(1 + t)) + (u < 0);
  auto v_check = _b.b.CreateICmpSLT(v, zero);
  auto u_check = _b.b.CreateICmpSLT(u, zero);
  auto sign = _b.b.CreateICmpEQ(v_check, u_check);
  auto u_abs = _b.b.CreateSelect(u_check, _b.b.CreateSub(zero, u), u);

  auto t = _b.b.CreateSelect(v_check, _b.b.CreateSub(minus_one, v), v);
  t = _b.b.CreateSDiv(t, u_abs);
  return _b.b.CreateAdd(
      _b.b.CreateSelect(sign, t, _b.b.CreateSub(minus_one, t)),
      b2i(Value(v.type, u_check)));
}

Value IrGenerator::binary(
    const Node& node, const Value& left, const Value& right)
{
  // If both scalar or vector, sizes must be equal, and we can directly operate
  // on the values.
  if (left.type.is_vector() == right.type.is_vector()) {
    return raw_binary(node, left, right);
  }

  // Otherwise one is a scalar and one a vector (but having the same base type),
  // and we need to extend the scalar to match the size of the vector.
  const Value& vector = left.type.is_vector() ? left : right;
  const Value& single = left.type.is_vector() ? right : left;
  Value v = _b.default_for_type(vector.type);

  for (std::size_t i = 0; i < vector.type.get_vector_size(); ++i) {
    v.irval = _b.b.CreateInsertElement(v, single, _b.constant_int(i));
  }
  return left.type.is_vector() ?
      raw_binary(node, vector, v) : raw_binary(node, v, vector);
}

Value IrGenerator::fold(const Node& node, const Value& value,
                        bool to_bool, bool with_ands, bool right_assoc)
{
  yang::Type etype =
      value.type.is_int_vector() ? yang::Type::int_t() : yang::Type::float_t();

  // Convert each argument to boolean, if necessary.
  std::vector<Value> elements;
  for (std::size_t i = 0; i < value.type.get_vector_size(); ++i) {
    Value v(etype, _b.b.CreateExtractElement(value, _b.constant_int(i)));
    elements.push_back(to_bool ? i2b(v) : v);
  }

  // Usually, we just form the chain (((e0 op e1) op e2) ...).
  if (!with_ands) {
    if (right_assoc) {
      auto it = elements.rbegin();
      Value v = *it++;
      for (; it != elements.rend(); ++it) {
        v = raw_binary(node, *it, v);
      }
      return v;
    }
    auto it = elements.begin();
    Value v = *it++;
    for (; it != elements.end(); ++it) {
      v = raw_binary(node, v, *it);
    }
    return v;
  }

  // For comparisons that isn't very useful, so instead form the chain
  // (e0 op e1) && (e1 op e2) && ...
  std::vector<Value> comparisons;
  for (std::size_t i = 1; i < elements.size(); ++i) {
    comparisons.push_back(raw_binary(node, elements[i - 1], elements[i]));
  }

  // Ignore right_assoc, since logical AND is associative.
  Value v = comparisons[0];
  for (std::size_t i = 1; i < comparisons.size(); ++i) {
    v.irval = _b.b.CreateAnd(v, comparisons[i]);
  }
  return v;
}

// End namespace yang::internal.
}
}
