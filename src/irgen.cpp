//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irgen.h"

#include <llvm/IR/Module.h>
#include <yang/context.h>
#include <yang/refcounting.h>

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

// TODO: this whole file needs refactored, to use a wrapped Value class that
// knows about Yang types instead of LLVM types.
// In particular, for example, we could store functions as two void pointers
// rather than two pointers where the first has type information (even though
// it might not actually be that type).
IrGenerator::IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
                         symbol_frame& globals, const Context& context)
  : IrCommon(module, engine)
  , _module(module)
  , _context(context)
  , _symbol_table(Value())
  , _metadata(nullptr)
  , _scope_to_function_map{{0, 0}}
  , _function_scope(0)
  , _update_refcount(get_native_function(
      "update_refcount", (void_fp)&update_refcount,
      llvm::FunctionType::get(
          _b.void_type(),
          std::vector<llvm::Type*>{
              _b.void_ptr_type(), _b.void_ptr_type(), _b.int_type()},
          false)))
  , _cleanup_structures(get_native_function(
      "cleanup_structures", (void_fp)&cleanup_structures,
      llvm::FunctionType::get(_b.void_type(), false)))
  , _destroy_internals(get_native_function(
      "destroy_internals", (void_fp)&destroy_internals,
      llvm::FunctionType::get(_b.void_type(), _b.void_ptr_type(), false)))
{
  // Set up the global data type. Since each program is designed to support
  // multiple independent instances running simultaeneously, the idea is to
  // define a structure type with a field for each global variable. Each
  // function will take a pointer to the global data structure as an implicit
  // final parameter.
  _global_data = init_structure_type(globals, "global_data");

  // We need to generate a reverse trampoline function for each function in the
  // Context. User type member functions are present in the context function map
  // as well as free functions.
  for (const auto& pair : context.get_functions()) {
    get_reverse_trampoline_function(pair.second.type, false);
  }
}

IrGenerator::~IrGenerator()
{
  // Keep hash<metadata> in source file.
}

void IrGenerator::emit_global_functions()
{
  // Create allocator function. It takes a pointer to the Yang program instance.
  auto alloc = llvm::Function::Create(
      llvm::FunctionType::get(_global_data.type, false),
      llvm::Function::ExternalLinkage, "!global_alloc", &_module);
  auto alloc_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", alloc);
  _b.b.SetInsertPoint(alloc_block);

  // TODO: here we call the global initialisation functions. It should also be
  // possible to define custom destructor functions that are called when the
  // global structure is freed.
  llvm::Value* v = allocate_structure_value(_global_data);
  for (llvm::Function* f : _global_inits) {
    _b.b.CreateCall(f, v);
  }
  _b.b.CreateRet(v);

  // Create accessor functions for each field of the global structure.
  for (const auto pair : _global_data.table) {
    llvm::Type* t = _b.get_llvm_type(pair.second.type);

    std::string name = "!global_get_" + pair.first;
    auto function_type = llvm::FunctionType::get(t, _global_data.type, false);
    auto getter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
    get_trampoline_function(
        yang::Type::function_t(pair.second.type, {}), false);

    auto getter_block = llvm::BasicBlock::Create(
        _b.b.getContext(), "entry", getter);
    auto it = getter->arg_begin();
    it->setName("global");
    _b.b.SetInsertPoint(getter_block);
    _b.b.CreateRet(memory_load(
        pair.second.type, structure_ptr(&*it, pair.second.index)));

    name = "!global_set_" + pair.first;
    std::vector<llvm::Type*> setter_args{t, _global_data.type};
    function_type = llvm::FunctionType::get(_b.void_type(), setter_args, false);
    auto setter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_module);
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
    memory_store(Value(pair.second.type, &*it),
                 structure_ptr(&*jt, pair.second.index));
    _b.b.CreateRetVoid();
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
          llvm::FunctionType::get(_b.void_type(), _global_data.type, false),
          llvm::Function::InternalLinkage, "!global_init", &_module);
      _global_inits.push_back(function);
      auto block =
          llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);

      // Store a special entry in the symbol table for the implicit global
      // structure pointer.
      auto it = function->arg_begin();
      it->setName("env");
      _metadata.add(ENVIRONMENT_PTR, &*it);
      _metadata.add(FUNCTION, function);
      _metadata.add(PARENT_BLOCK, _b.b.GetInsertBlock());
      allocate_closure_struct(node.static_info.closed_environment, &*it);

      _metadata.add(GLOBAL_INIT_FUNCTION, function);
      _b.b.SetInsertPoint(block);
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
      _refcount_loop_indices.push_back(_refcount_locals.back().size());
      _refcount_locals.back().emplace_back();
      auto loop_block = create_block(LOOP_BODY_BLOCK, "loop");
      auto cond_block = create_block(LOOP_COND_BLOCK, "cond");
      auto merge_block = create_block(MERGE_BLOCK, "merge");
      _metadata.add(LOOP_BREAK_LABEL, merge_block);
      _metadata.add(LOOP_CONTINUE_LABEL, cond_block);

      _b.b.CreateBr(loop_block);
      _b.b.SetInsertPoint(loop_block);
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
      create_function(node, results[0].type);
      break;
    }

    case Node::IF_STMT:
    {
      auto then_block = (llvm::BasicBlock*)_metadata[IF_THEN_BLOCK];
      auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];

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
      auto cond_block = (llvm::BasicBlock*)_metadata[LOOP_COND_BLOCK];
      auto loop_block = (llvm::BasicBlock*)_metadata[LOOP_BODY_BLOCK];
      auto after_block = (llvm::BasicBlock*)_metadata[LOOP_AFTER_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];

      if (results.size() == 1) {
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(cond_block);
        _refcount_loop_indices.push_back(_refcount_locals.back().size());
        _refcount_locals.back().emplace_back();
      }
      if (results.size() == 2) {
        auto parent = _b.b.GetInsertBlock()->getParent();
        auto clean_block =
            llvm::BasicBlock::Create(_b.b.getContext(), "clean", parent);
        _b.b.CreateCondBr(i2b(results[1]), loop_block, clean_block);
        _b.b.SetInsertPoint(clean_block);
        dereference_scoped_locals();
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
      auto cond_block = (llvm::BasicBlock*)_metadata[LOOP_COND_BLOCK];
      dereference_scoped_locals();
      _refcount_loop_indices.pop_back();
      _refcount_locals.back().pop_back();
      _symbol_table.pop();
      _symbol_table.push();
      _refcount_locals.back().emplace_back();
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
        _metadata.push();

        // Blocks and branching are necessary (as opposed to a select
        // instruction) to short-circuit and avoiding evaluating the other path.
        auto then_block = create_block(IF_THEN_BLOCK, "then");
        auto else_block = create_block(IF_ELSE_BLOCK, "else");
        create_block(MERGE_BLOCK, "merge");

        _b.b.CreateCondBr(i2b(results[0]), then_block, else_block);
        _b.b.SetInsertPoint(then_block);
      }
      if (results.size() == 2) {
        auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
        auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
        _b.b.CreateBr(merge_block);

        // Metadata blocks must always be updated to the current one, in case we
        // created a bunch of new ones before we got to the end!
        _metadata[IF_THEN_BLOCK] = _b.b.GetInsertBlock();
        _b.b.SetInsertPoint(else_block);
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
      _metadata.push();
      _metadata.add(LOGICAL_OP_SOURCE_BLOCK, &*_b.b.GetInsertPoint());
      auto rhs_block = create_block(LOGICAL_OP_RHS_BLOCK, "rhs");
      auto merge_block = create_block(MERGE_BLOCK, "merge");

      if (node.type == Node::LOGICAL_OR) {
        _b.b.CreateCondBr(i2b(results[0]), merge_block, rhs_block);
      }
      else {
        _b.b.CreateCondBr(i2b(results[0]), rhs_block, merge_block);
      }
      _b.b.SetInsertPoint(rhs_block);
      break;
    }

    default: {}
  }
}

Value IrGenerator::visit(const Node& node, const result_list& results)
{
  auto parent = _b.b.GetInsertBlock() ?
      _b.b.GetInsertBlock()->getParent() : nullptr;

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
      dereference_scoped_locals(0);
      if (!node.static_info.closed_environment.empty()) {
        _scope_closures.pop_back();
      }
      _b.b.CreateRetVoid();
      _symbol_table.pop();
      _metadata.pop();
      _refcount_locals.pop_back();
      return results[0];
    case Node::GLOBAL_ASSIGN:
    {
      auto function = (llvm::Function*)parent;
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
      if (results[0].type.get_function_return_type().is_void()) {
        dereference_scoped_locals(0);
        if (_metadata[CLOSURE_PTR]) {
          update_reference_count(nullptr, _metadata[CLOSURE_PTR], -1);
        }
        _b.b.CreateRetVoid();
      }
      else {
        // In a function that passes the static check, control never reaches
        // this point; but the block must have a terminator.
        _b.b.CreateBr(_b.b.GetInsertBlock());
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
        // Global functions need a trampoline.
        get_trampoline_function(results[0].type, false);
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
      auto closure = _metadata[CLOSURE_PTR];
      return _b.function_value(
          results[0].type,
          parent, closure ? closure : _metadata[ENVIRONMENT_PTR]);
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
      _symbol_table.pop();
      dereference_scoped_locals();
      _refcount_locals.back().pop_back();
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
      dereference_scoped_locals(0);
      if (_metadata[CLOSURE_PTR]) {
        update_reference_count(nullptr, _metadata[CLOSURE_PTR], -1);
      }
      node.type == Node::RETURN_STMT ?
          _b.b.CreateRet(results[0]) : _b.b.CreateRetVoid();
      _b.b.SetInsertPoint(dead_block);
      return node.type == Node::RETURN_STMT ? results[0] : Value();
    }
    case Node::IF_STMT:
    {
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();

      _b.b.CreateBr(merge_block);
      _b.b.SetInsertPoint(merge_block);
      dereference_scoped_locals();
      _refcount_locals.back().pop_back();
      return results[0];
    }
    case Node::FOR_STMT:
    {
      auto after_block = (llvm::BasicBlock*)_metadata[LOOP_AFTER_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _symbol_table.pop();
      _metadata.pop();
      dereference_scoped_locals();
      _refcount_loop_indices.pop_back();
      _refcount_locals.back().pop_back();

      _b.b.CreateBr(after_block);
      _b.b.SetInsertPoint(merge_block);
      dereference_scoped_locals();
      _refcount_locals.back().pop_back();
      return results[0];
    }
    case Node::DO_WHILE_STMT:
    {
      auto loop_block = (llvm::BasicBlock*)_metadata[LOOP_BODY_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();
      dereference_scoped_locals();
      _refcount_locals.back().pop_back();

      _b.b.CreateCondBr(i2b(results[1]), loop_block, merge_block);
      _b.b.SetInsertPoint(merge_block);
      return results[0];
    }
    case Node::BREAK_STMT:
    case Node::CONTINUE_STMT:
    {
      dereference_scoped_locals(_refcount_loop_indices.back());
      auto dead_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "dead", parent);
      _b.b.CreateBr((llvm::BasicBlock*)_metadata[
          node.type == Node::BREAK_STMT ? LOOP_BREAK_LABEL :
                                          LOOP_CONTINUE_LABEL]);
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
      // Just return the object directly; the call site has special logic to
      // deal with member functions.
      return results[0];

    case Node::IDENTIFIER:
    {
      // In type-context, we just want to return a user type.
      if (_metadata.has(TYPE_EXPR_CONTEXT)) {
        return yang::Type::user_t();
      }

      // Load the local variable, if it's there.
      if (_symbol_table.has(node.string_value) &&
          _symbol_table.index(node.string_value)) {
        return memory_load(_symbol_table[node.string_value].type,
                           get_variable_ptr(node.string_value));
      }
      // Otherwise it's a top-level function, global, or context function. We
      // must be careful to only return globals/functions *after* they have been
      // defined, otherwise it might be a reference to a context function of the
      // same name.
      if (_symbol_table.has(node.string_value)) {
        // If the symbol table entry is non-null it's a top-level function; just
        // get the value.
        if (_symbol_table.get(node.string_value, 0)) {
          return _b.function_value(
              _symbol_table.get(node.string_value, 0).type,
              _symbol_table.get(node.string_value, 0),
              global_ptr());
        }
        // Otherwise it's a global, so look up in the global structure with a
        // byte-aligned load.
        return memory_load(_symbol_table.get(node.string_value, 0).type,
                           global_ptr(node.string_value));
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
        return Value(results[1].type,
                     _b.b.CreateSelect(i2b(results[0]), results[1], results[2]));
      }
      // Update in case we branched again.
      _metadata[IF_ELSE_BLOCK] = _b.b.GetInsertBlock();

      auto then_block = (llvm::BasicBlock*)_metadata[IF_THEN_BLOCK];
      auto else_block = (llvm::BasicBlock*)_metadata[IF_ELSE_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();
      _b.b.CreateBr(merge_block);
      _b.b.SetInsertPoint(merge_block);
      auto phi = _b.b.CreatePHI(results[1].llvm_type(), 2, "tern");
      phi->addIncoming(results[1], then_block);
      phi->addIncoming(results[2], else_block);
      return Value(results[1].type, phi);
    }
    case Node::CALL:
    {
      if (_metadata.has(TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }
      std::vector<llvm::Value*> args;
      Value genf = results[0];

      // Special logic for member-function calling.
      // TODO: get rid of this now that we have closures.
      if (node.children[0]->type == Node::MEMBER_SELECTION) {
        std::string s = node.children[0]->static_info.user_type_name +
            "::" + node.children[0]->string_value;
        auto it = _context.get_functions().find(s);
        genf = _b.function_value(it->second);

        args.push_back(results[0]);
      }
      for (std::size_t i = 1; i < results.size(); ++i) {
        args.push_back(results[i]);
      }

      // Extract pointers from the struct.
      llvm::Value* fptr = _b.b.CreateExtractValue(genf, 0, "fptr");
      llvm::Value* eptr = _b.b.CreateExtractValue(genf, 1, "eptr");
      args.push_back(eptr);

      // Switch on the presence of a trampoline function pointer.
      auto cpp_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "cpp", parent);
      auto yang_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "yang", parent);
      auto merge_block =
          llvm::BasicBlock::Create(_b.b.getContext(), "merge", parent);

      llvm::Value* cmp = _b.b.CreateICmpEQ(
          eptr, llvm::ConstantPointerNull::get(_b.void_ptr_type()));
      _b.b.CreateCondBr(cmp, cpp_block, yang_block);

      _b.b.SetInsertPoint(yang_block);
      llvm::Value* cast = _b.b.CreateBitCast(
          fptr, llvm::PointerType::get(_b.raw_function_type(genf.type), 0));
      llvm::Value* yang_val = _b.b.CreateCall(cast, args);
      _b.b.CreateBr(merge_block);

      // Don't bother to generate correct code when the C++ path can never be
      // taken.
      llvm::Value* trampoline =
          get_reverse_trampoline_function(genf.type, false);
      llvm::Value* cpp_val = nullptr;
      _b.b.SetInsertPoint(cpp_block);
      if (trampoline) {
        args.push_back(_b.b.CreateBitCast(fptr, _b.void_ptr_type(), "cast"));
        cpp_val = _b.b.CreateCall(trampoline, args);
        _b.b.CreateBr(merge_block);
      }
      else {
        _b.b.CreateBr(yang_block);
      }

      _b.b.SetInsertPoint(merge_block);
      const yang::Type& return_t = genf.type.get_function_return_type();
      if (!return_t.is_void()) {
        auto phi_v = _b.b.CreatePHI(_b.get_llvm_type(return_t), 2, "call");
        Value phi(return_t, phi_v);

        if (trampoline) {
          phi_v->addIncoming(cpp_val, cpp_block);
        }
        phi_v->addIncoming(yang_val, yang_block);
        // Reference count the temporary.
        update_reference_count(phi, 1);
        _refcount_locals.back().back().push_back(phi);
        return phi;
      }
      return Value();
    }

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    {
      if (results[0].type.is_vector()) {
        // Short-circuiting isn't possible.
        return b2i(binary(node, i2b(results[0]), i2b(results[1])));
      }
      // Update in case we branched again.
      _metadata[LOGICAL_OP_RHS_BLOCK] = _b.b.GetInsertBlock();

      auto source_block =
          (llvm::BasicBlock*)_metadata[LOGICAL_OP_SOURCE_BLOCK];
      auto rhs_block = (llvm::BasicBlock*)_metadata[LOGICAL_OP_RHS_BLOCK];
      auto merge_block = (llvm::BasicBlock*)_metadata[MERGE_BLOCK];
      _metadata.pop();

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

      auto phi = _b.b.CreatePHI(type, 2, "cut");
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
      Value v = results[0].type.is_vector() ?
          _b.constant_int_vector(0, results[0].type.get_vector_size()) :
          _b.constant_int(0);
      v.irval = _b.b.CreateICmpEQ(results[0], v, "lneg");
      return b2i(v);
    }
    case Node::BITWISE_NEGATION:
    {
      Value v = results[0].type.is_vector() ?
        _b.constant_int_vector(0u - 1, results[0].type.get_vector_size()) :
        _b.constant_int(0u - 1);
      v.irval = _b.b.CreateXor(results[0], v, "neg");
      return v;
    }
    case Node::ARITHMETIC_NEGATION:
    {
      Value v;
      if (results[0].type.is_vector()) {
        v = results[0].type.is_int_vector() ?
            _b.constant_int_vector(0, results[0].type.get_vector_size()) :
            _b.constant_float_vector(
                0, results[0].type.get_vector_size());
      }
      else {
        v = results[0].type.is_int() ?
            _b.constant_int(0) : _b.constant_float(0);
      }
      v.irval = results[0].type.is_int() || results[0].type.is_int_vector() ?
          _b.b.CreateSub(v, results[0], "sub") :
          _b.b.CreateFSub(v, results[0], "fsub");
      return v;
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
        _symbol_table.add(s, 0, Value(results[1].type));
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
        auto llvm_function =
            (llvm::Function*)_b.b.GetInsertPoint()->getParent();
        auto& entry_block = llvm_function->getEntryBlock();
        llvm::IRBuilder<> entry(&entry_block, entry_block.begin());
        storage = entry.CreateAlloca(
            _b.get_llvm_type(results[1].type), nullptr, s);
        memory_init(entry, storage);
        _refcount_locals.back().back().push_back(
            Value(results[1].type, storage));
      }

      memory_store(results[1], storage);
      _symbol_table.add(s, Value(results[1].type, storage));
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
            v.irval, results[i], _b.constant_int(i), "vec");
      }
      return v;
    }
    case Node::VECTOR_INDEX:
    {
      // Indexing out-of-bounds produces constant zero.
      Value v = results[0].type.is_int_vector() ?
          _b.constant_int(0) : _b.constant_float(0);
      auto ge = _b.b.CreateICmpSGE(results[1], _b.constant_int(0), "idx");
      auto lt = _b.b.CreateICmpSLT(
          results[1],
          _b.constant_int(results[0].type.get_vector_size()), "idx");

      auto in = _b.b.CreateAnd(ge, lt, "idx");
      v.irval =_b.b.CreateSelect(
          in, _b.b.CreateExtractElement(results[0], results[1], "idx"), v,
          "idx");
      return v;
    }

    default:
      return Value();
  }
}

IrGenerator::structure_t IrGenerator::init_structure_type(
    const symbol_frame& symbols, const std::string& name)
{
  structure_t st;
  // Since the structure may contain pointers to functions which take the type
  // itself as an argument, the variable must be set (to an opaque type).
  auto struct_type =
      llvm::StructType::create(_module.getContext(), name);
  st.type = llvm::PointerType::get(struct_type, 0);
  auto free_type = llvm::FunctionType::get(_b.void_type(), st.type, false);
  std::vector<llvm::Type*> query_args{
      st.type, llvm::PointerType::get(_b.void_ptr_type(), 0)};
  auto query_type = llvm::FunctionType::get(_b.void_type(), query_args, false);
  std::vector<llvm::Type*> type_list;

  // The first element in closure structures is a pointer to the parent
  // structure. For global data, this will be null.
  type_list.push_back(_b.void_ptr_type());
  // The second element is a pointer to the reference counter.
  type_list.push_back(_b.int_type());
  // The third is a pointer to the destructor.
  type_list.push_back(llvm::PointerType::get(free_type, 0));
  // The fourth is the number of outgoing references this structure has.
  type_list.push_back(_b.int_type());
  // The fifth is the pointer to the reference query function.
  type_list.push_back(llvm::PointerType::get(query_type, 0));

  std::size_t number = 5;
  for (const auto& pair : symbols) {
    // Type-calculation must be kept up-to-date with new types, or globals of
    // that type will fail.
    type_list.push_back(_b.get_llvm_type(pair.second));
    st.table[pair.first] = structure_entry(pair.second, number++);
  }
  struct_type->setBody(type_list, false);
  bool global_data = name == "global_data";

  // Create the destruction function.
  auto prev_block = _b.b.GetInsertBlock();
  st.destructor = llvm::Function::Create(
      free_type, llvm::Function::InternalLinkage, "!free_" + name, &_module);
  auto free_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "entry", st.destructor);
  _b.b.SetInsertPoint(free_block);
  auto it = st.destructor->arg_begin();
  it->setName("struct");
  // Make sure to decrement the reference count on each global variable on
  // destruction.
  for (const auto& pair : st.table) {
    Value old = memory_load(pair.second.type,
                            structure_ptr(&*it, pair.second.index));
    update_reference_count(old, -1);
  }
  llvm::Value* parent =
      memory_load(yang::Type::void_t(), structure_ptr(&*it, 0));
  if (global_data) {
    _b.b.CreateCall(_destroy_internals, parent);
  }
  else {
    update_reference_count(nullptr, parent, -1);
  }
  _b.b.CreateRetVoid();

  // Create the reference query function.
  st.refout_query = llvm::Function::Create(
      query_type, llvm::Function::InternalLinkage, "!query_" + name, &_module);
  auto query_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "entry", st.refout_query);
  it = st.refout_query->arg_begin();
  auto jt = it;
  ++jt;
  it->setName("struct");
  jt->setName("output");

  _b.b.SetInsertPoint(query_block);
  // Output pointers for each function value environment.
  for (const auto& pair : st.table) {
    if (!pair.second.type.is_function()) {
      continue;
    }
    llvm::Value* f = memory_load(
        pair.second.type, structure_ptr(&*it, pair.second.index));
    llvm::Value* eptr = _b.b.CreateExtractValue(f, 1, "eptr");
    std::vector<llvm::Value*> indices{_b.constant_int(st.refout_count)};
    _b.b.CreateAlignedStore(eptr, _b.b.CreateGEP(jt, indices, "out"), 1);
    ++st.refout_count;
  }
  // Also output the parent pointer.
  if (!global_data) {
    llvm::Value* v = memory_load(yang::Type::void_t(), structure_ptr(&*it, 0));
    std::vector<llvm::Value*> indices{_b.constant_int(st.refout_count)};
    _b.b.CreateAlignedStore(v, _b.b.CreateGEP(jt, indices, "out"), 1);
    ++st.refout_count;
  }
  _b.b.CreateRetVoid();

  _b.b.SetInsertPoint(prev_block);
  return st;
}

llvm::Value* IrGenerator::allocate_structure_value(const structure_t& st)
{
  llvm::Type* llvm_size_t =
      llvm::IntegerType::get(_b.b.getContext(), 8 * sizeof(std::size_t));

  _b.b.CreateCall(_cleanup_structures);
  auto malloc_ptr = get_native_function(
      "malloc", (yang::void_fp)&malloc,
      llvm::FunctionType::get(st.type, llvm_size_t, false));

  // Compute sizeof(type) by indexing one past the null pointer.
  llvm::Value* size_of =
      _b.b.CreateIntToPtr(_b.constant_int(0), st.type, "null");
  size_of = _b.b.CreateGEP(size_of, _b.constant_int(1), "sizeof");
  size_of = _b.b.CreatePtrToInt(size_of, llvm_size_t, "size");

  // Call malloc, make sure refcounted memory is initialised, and return the
  // pointer.
  llvm::Value* v = _b.b.CreateCall(malloc_ptr, size_of, "call");
  memory_store(Value(yang::Type::void_t(), _b.constant_ptr(nullptr)),
               structure_ptr(v, 0));

  memory_store(_b.constant_int(0), structure_ptr(v, 1));
  memory_store(Value(yang::Type::void_t(), st.destructor),
               structure_ptr(v, 2));

  memory_store(_b.constant_int(st.refout_count), structure_ptr(v, 3));
  memory_store(Value(yang::Type::void_t(), st.refout_query),
               structure_ptr(v, 4));
  for (const auto& pair : st.table) {
    memory_init(_b.b, structure_ptr(v, pair.second.index));
  }
  return v;
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
  structure_t closure_st = init_structure_type(symbols, "closure");
  llvm::Value* closure_value = allocate_structure_value(closure_st);
  // Store in metadata.
  _metadata.add(CLOSURE_PTR, closure_value);
  _scope_closures.push_back(closure_st);

  // Store parent pointer in the first slot.
  auto parent_void_ptr =
      _b.b.CreateBitCast(parent_ptr, _b.void_ptr_type());
  memory_store(Value(yang::Type::void_t(), parent_void_ptr),
               structure_ptr(closure_value, 0));
  update_reference_count(nullptr, parent_void_ptr, 1);
  // Set up the symbol-table for all the rest. If a closed variable "v"
  // appears in scope #1, for instance, we store "v/1" in the symbol table
  // with a pointer into the closure-structure.
  //
  // Then, when we reach the actual declaration of v in scope #1, we store
  // into "v/1" and copy the symbol table entry for "v/1" to "v" for that
  // scope and its children (as in the argument list code below).
  for (const auto& pair : closure_st.table) {
    llvm::Value* ptr =
        structure_ptr(closure_value, pair.second.index);
    _symbol_table.add(pair.first, Value(pair.second.type, ptr));
  }
  return closure_value;
}

llvm::Value* IrGenerator::get_parent_struct(
    std::size_t parent_steps, llvm::Value* v)
{
  llvm::Type* void_ptr_ptr = llvm::PointerType::get(
      llvm::StructType::create(_b.b.getContext(), _b.void_ptr_type()), 0);
  llvm::Value* u = v;
  for (std::size_t i = 0; i < parent_steps; ++i) {
    u = memory_load(
        yang::Type::void_t(),
        structure_ptr(_b.b.CreateBitCast(u, void_ptr_ptr), 0));
  }
  return u;
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
      _scope_closures[closure_index].table[unique_name].index;
  return structure_ptr(
      _b.b.CreateBitCast(closure_ptr, closure_type), struct_index);
}

void IrGenerator::create_function(
    const Node& node, const yang::Type& function_type)
{
  _metadata.push();

  // Linkage will be set later if necessary.
  auto llvm_type = _b.raw_function_type(function_type);
  auto function = llvm::Function::Create(
      // TODO: this should be internal linkage and set external later, but that
      // doesn't work for some reason.
      llvm_type, llvm::Function::ExternalLinkage,
      "anonymous", &_module);

  // The code for Node::TYPE_FUNCTION in visit() ensures it takes an environment
  // pointer.
  auto eptr = --function->arg_end();
  eptr->setName("env");
  _metadata.add(ENVIRONMENT_PTR, &*eptr);
  _metadata.add(FUNCTION, function);
  _metadata.add(PARENT_BLOCK, &*_b.b.GetInsertPoint());

  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);
  _b.b.SetInsertPoint(block);

  // Increase depth of scope pointers if the parent function created a closure.
  if (_metadata[CLOSURE_PTR]) {
    _scope_to_function_map.emplace(_symbol_table.size(), ++_function_scope);
  }
  _symbol_table.push();
  // Some optimisations may be possible. For example, const variables may not
  // need to go in the closure structure. Maybe LLVM can handle that anyway?
  //
  // Also, we naively allocate at most one closure structure per function
  // invocation. Obviously, if we could partition the inner functions such that
  // the sets of enclosing variables they access are disjoint, we could allocate
  // separate structures for each (and potentially return unused memory sooner).
  allocate_closure_struct(node.static_info.closed_environment, &*eptr);
  // Refcounting on closure pointer.
  if (_metadata[CLOSURE_PTR]) {
    update_reference_count(nullptr, _metadata[CLOSURE_PTR], 1);
  }

  // Lambda for deciding whether to put an argument in closure or stack.
  auto assign_storage = [&](
      const yang::Type& type, const std::string& name, std::int32_t scope_mod)
  {
    std::string unique_name =
        name + "/" + std::to_string(node.static_info.scope_number + scope_mod);

    if (_symbol_table.has(unique_name)) {
      llvm::Value* v = _symbol_table[unique_name];
      _value_to_unique_name_map.emplace(v, unique_name);
      return v;
    }
    // Rather than reference argument values directly, we create an alloca
    // and store the argument in there. This simplifies things, since we
    // can emit the same IR code when referencing local variables or
    // function arguments.
    llvm::Value* v = _b.b.CreateAlloca(_b.get_llvm_type(type), nullptr, name);
    _refcount_locals.back().back().push_back(Value(type, v));
    memory_init(_b.b, v);
    return v;
  };

  // Store the function's name in its own scope. Recursive lookup handled
  // similarly to arguments below.
  if (_immediate_left_assign.length()) {
    function->setName(_immediate_left_assign);
    // The identifier is registered one scope above the function argument scope.
    // Confusingly, that's two unique scope-numbers back because the LHS of the
    // assignment has its own scope in-between.
    llvm::Value* storage =
        assign_storage(function_type, _immediate_left_assign, -2);
    memory_store(_b.function_value(function_type, function, eptr), storage);
    _symbol_table.add(_immediate_left_assign, Value(function_type, storage));
    _immediate_left_assign.clear();
  }
  _symbol_table.push();

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
    memory_store(Value(arg, &*it), storage);
    _symbol_table.add(name, Value(arg, storage));
  }
}

Value IrGenerator::i2b(const Value& v)
{
  Value result = v.type.is_vector() ?
      _b.constant_int_vector(0, v.type.get_vector_size()) :
      _b.constant_int(0);
  result.irval = _b.b.CreateICmpNE(v, result, "bool");
  return result;
}

Value IrGenerator::b2i(const Value& v)
{
  if (v.type.is_vector()) {
    std::size_t size = v.type.get_vector_size();
    return Value(yang::Type::int_vector_t(size),
                 _b.b.CreateZExt(v, _b.int_vector_type(size), "int"));
  }
  return Value(yang::Type::int_t(), _b.b.CreateZExt(v, _b.int_type()));
}

Value IrGenerator::i2f(const Value& v)
{
  if (v.type.is_vector()) {
    std::size_t size = v.type.get_vector_size();
    return Value(yang::Type::float_vector_t(size),
                 _b.b.CreateSIToFP(v, _b.float_vector_type(size), "flt"));
  }
  return Value(
      yang::Type::float_t(), _b.b.CreateSIToFP(v, _b.float_type(), "flt"));
}

Value IrGenerator::f2i(const Value& v)
{
  llvm::Type* back_type = _b.float_type();
  llvm::Type* type = _b.int_type();
  Value zero;

  std::size_t size = v.type.get_vector_size();
  if (v.type.is_vector()) {
    back_type = _b.float_vector_type(size);
    type = _b.int_vector_type(size);
    zero = _b.constant_float_vector(0, size);
  }
  else {
    zero = _b.constant_float(0);
  }

  // Mathematical floor. Implements the algorithm:
  // return int(v) - (v < 0 && v != float(int(v)));
  auto cast = _b.b.CreateFPToSI(v, type, "int");
  auto back = _b.b.CreateSIToFP(cast, back_type, "int");

  auto a_check = _b.b.CreateFCmpOLT(v, zero, "int");
  auto b_check = _b.b.CreateFCmpONE(v, back, "int");
  auto and_v = b2i(Value(yang::Type::int_vector_t(zero.type.get_vector_size()),
                         _b.b.CreateAnd(a_check, b_check, "int")));
  return Value(
      v.type.is_vector() ? yang::Type::int_vector_t(size) : yang::Type::int_t(),
      _b.b.CreateSub(cast, and_v, "int"));
}

llvm::Value* IrGenerator::structure_ptr(llvm::Value* ptr, std::size_t index)
{
  // The first index indexes the structure data pointer itself, i.e. to obtain
  // the one and only global data structure at that memory location.
  std::vector<llvm::Value*> indexes{_b.constant_int(0), _b.constant_int(index)};
  return _b.b.CreateGEP(ptr, indexes, "index");
}

llvm::Value* IrGenerator::global_ptr(const std::string& name)
{
  return structure_ptr(global_ptr(), _global_data.table[name].index);
}

llvm::Value* IrGenerator::global_ptr()
{
  // This always gets the global pointer, even in inner functions of closure
  // scopes.
  llvm::Value* ptr = get_parent_struct(
      _scope_to_function_map.rbegin()->second, _metadata[ENVIRONMENT_PTR]);
  // Bitcast, since it's represented as void pointer.
  return _b.b.CreateBitCast(ptr, _global_data.type);
}

Value IrGenerator::memory_load(const yang::Type& type, llvm::Value* ptr)
{
  // Loads out of the global data structure must be byte-aligned! I don't
  // entirely understand why, but leaving the default will segfault at random
  // sometimes for certain types (e.g. float vectors).
  return Value(type, _b.b.CreateAlignedLoad(ptr, 1, "load"));
}

void IrGenerator::memory_init(llvm::IRBuilder<>& pos, llvm::Value* ptr)
{
  // We need to make sure ref-counted memory locations are initialised with
  // something sensible. Otherwise, the first store will try to decrement
  // the refcount on something undefined. (Important in particular for variable
  // declarations which are "executed" more than once.)
  llvm::Type* elem = ptr->getType()->getPointerElementType();
  if (elem->isStructTy()) {
    // Null Yang function. Passing void as type is OK since it isn't used.
    pos.CreateAlignedStore(
        _b.function_value_null(yang::Type::void_t()), ptr, 1);
  }
}

void IrGenerator::memory_store(const Value& value, llvm::Value* ptr)
{
  Value old = memory_load(value.type, ptr);
  update_reference_count(old, -1);
  update_reference_count(value, 1);
  _b.b.CreateAlignedStore(value, ptr, 1);
}

void IrGenerator::update_reference_count(const Value& value, int_t change)
{
  if (!value.type.is_function()) {
    return;
  }
  // TODO: for speed, Yang reference counting at least should be inlined.
  llvm::Value* fptr = _b.b.CreateExtractValue(value, 0, "fptr");
  llvm::Value* eptr = _b.b.CreateExtractValue(value, 1, "eptr");
  update_reference_count(fptr, eptr, change);
}

void IrGenerator::update_reference_count(
    llvm::Value* fptr, llvm::Value* eptr, int_t change)
{
  fptr = fptr ?
      _b.b.CreateBitCast(fptr, _b.void_ptr_type()) : _b.constant_ptr(nullptr);
  eptr = eptr ?
      _b.b.CreateBitCast(eptr, _b.void_ptr_type()) : _b.constant_ptr(nullptr);
  std::vector<llvm::Value*> args{fptr, eptr, _b.constant_int(change)};
  _b.b.CreateCall(_update_refcount, args);
}

void IrGenerator::dereference_scoped_locals()
{
  dereference_scoped_locals(_refcount_locals.back().size() - 1);
}

void IrGenerator::dereference_scoped_locals(std::size_t first_scope)
{
  const auto& function = _refcount_locals.back();
  for (std::size_t i = first_scope; i < function.size(); ++i) {
    for (const Value& v : function[i]) {
      if (!v.llvm_type()->isPointerTy()) {
        update_reference_count(v, -1);
        continue;
      }
      Value load = memory_load(v.type, v);
      update_reference_count(load, -1);
      memory_init(_b.b, v);
    }
  }
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
        type == Node::LOGICAL_OR ? _b.b.CreateOr(vi, ui, "lor") :
        type == Node::LOGICAL_AND ? _b.b.CreateAnd(vi, ui, "land") :
        type == Node::BITWISE_OR ? _b.b.CreateOr(vi, ui, "or") :
        type == Node::BITWISE_AND ? _b.b.CreateAnd(vi, ui, "and") :
        type == Node::BITWISE_XOR ? _b.b.CreateXor(vi, ui, "xor") :
        type == Node::BITWISE_LSHIFT ? _b.b.CreateShl(vi, ui, "lsh") :
        type == Node::BITWISE_RSHIFT ? _b.b.CreateAShr(vi, ui, "rsh") :
        type == Node::POW ? pow(v, u) :
        type == Node::MOD ? mod(v, u) :
        type == Node::ADD ? _b.b.CreateAdd(vi, ui, "add") :
        type == Node::SUB ? _b.b.CreateSub(vi, ui, "sub") :
        type == Node::MUL ? _b.b.CreateMul(vi, ui, "mul") :
        type == Node::DIV ? div(v, u) :
        type == Node::EQ ? _b.b.CreateICmpEQ(vi, ui, "eq") :
        type == Node::NE ? _b.b.CreateICmpNE(vi, ui, "ne") :
        type == Node::GE ? _b.b.CreateICmpSGE(vi, ui, "ge") :
        type == Node::LE ? _b.b.CreateICmpSLE(vi, ui, "le") :
        type == Node::GT ? _b.b.CreateICmpSGT(vi, ui, "gt") :
        type == Node::LT ? _b.b.CreateICmpSLT(vi, ui, "lt") :
        nullptr;
    return Value(v.type, out);
  }

  llvm::Value* out =
      type == Node::POW ? pow(v, u) :
      type == Node::MOD ? mod(v, u) :
      type == Node::ADD ? _b.b.CreateFAdd(vi, ui, "fadd") :
      type == Node::SUB ? _b.b.CreateFSub(vi, ui, "fsub") :
      type == Node::MUL ? _b.b.CreateFMul(vi, ui, "fmul") :
      type == Node::DIV ? div(v, u) :
      type == Node::EQ ? _b.b.CreateFCmpOEQ(vi, ui, "feq") :
      type == Node::NE ? _b.b.CreateFCmpONE(vi, ui, "fne") :
      type == Node::GE ? _b.b.CreateFCmpOGE(vi, ui, "fge") :
      type == Node::LE ? _b.b.CreateFCmpOLE(vi, ui, "fle") :
      type == Node::GT ? _b.b.CreateFCmpOGT(vi, ui, "fgt") :
      type == Node::LT ? _b.b.CreateFCmpOLT(vi, ui, "flt") :
      nullptr;
  return Value(v.type, out);
}

llvm::Value* IrGenerator::pow(const Value& v, const Value& u)
{
  std::vector<llvm::Type*> args{_b.float_type(), _b.float_type()};
  auto pow_ptr = get_native_function(
      "pow", (yang::void_fp)&::pow,
      llvm::FunctionType::get(_b.float_type(), args, false));

  if (!v.type.is_vector()) {
    Value vv = v.type.is_int() ? i2f(v) : v;
    Value uu = u.type.is_int() ? i2f(u) : u;

    std::vector<llvm::Value*> args{vv, uu};
    Value r(yang::Type::float_t(), _b.b.CreateCall(pow_ptr, args, "pow"));
    return v.type.is_int() ? f2i(r) : r;
  }

  Value result = _b.constant_float_vector(0, v.type.get_vector_size());
  for (std::size_t i = 0; i < v.type.get_vector_size(); ++i) {
    Value x(yang::Type::float_t(),
            _b.b.CreateExtractElement(v, _b.constant_int(i), "pow"));
    Value y(yang::Type::float_t(),
            _b.b.CreateExtractElement(u, _b.constant_int(i), "pow"));

    Value xx = v.type.is_int_vector() ? i2f(x) : x;
    Value yy = u.type.is_int_vector() ? i2f(y) : y;
    std::vector<llvm::Value*> args{xx, yy};

    llvm::Value* call = _b.b.CreateCall(pow_ptr, args, "pow");
    result.irval =
        _b.b.CreateInsertElement(result, call, _b.constant_int(i), "pow");
  }
  return v.type.is_int_vector() ? f2i(result) : result;
}

llvm::Value* IrGenerator::mod(const Value& v, const Value& u)
{
  if (!v.type.is_int() && !v.type.is_int_vector()) {
    return _b.b.CreateFRem(v, u, "fmod");
  }

  Value zero = v.type.is_int_vector() ?
      _b.constant_int_vector(0, v.type.get_vector_size()) : _b.constant_int(0);
  // Implements the following algorithm:
  // return (v >= 0 ? v : v + (bool(|v| % |u|) + |v| / |u|) * |u|) % |u|;
  // There are simpler ways, but they are vulnerable to overflow errors.
  // k = |v| % |u| + |v| / |u| is the smallest postive integer such that
  // k * |u| >= |v|.
  auto v_check = _b.b.CreateICmpSGE(v, zero, "mod");
  auto u_check = _b.b.CreateICmpSGE(u, zero, "mod");
  auto v_abs = _b.b.CreateSelect(
      v_check, v, _b.b.CreateSub(zero, v, "mod"), "mod");
  auto u_abs = _b.b.CreateSelect(
      u_check, u, _b.b.CreateSub(zero, u, "mod"), "mod");

  Value rem(zero.type, _b.b.CreateSRem(v_abs, u_abs, "mod"));
  auto k = _b.b.CreateAdd(b2i(i2b(rem)),
                          _b.b.CreateSDiv(v_abs, u_abs, "mod"), "mod");
  auto lhs = _b.b.CreateSelect(
      v_check, v,
      _b.b.CreateAdd(v, _b.b.CreateMul(k, u_abs, "mod"), "mod"), "mod");
  return _b.b.CreateSRem(lhs, u_abs, "mod");
}

llvm::Value* IrGenerator::div(const Value& v, const Value& u)
{
  if (!v.type.is_int() && !v.type.is_int_vector()) {
    return _b.b.CreateFDiv(v, u, "fdiv");
  }

  Value zero = v.type.is_int_vector() ?
      _b.constant_int_vector(0, v.type.get_vector_size()) : _b.constant_int(0);
  Value minus_one = v.type.is_int_vector() ?
      _b.constant_int_vector(-1, v.type.get_vector_size()) :
      _b.constant_int(-1);
  // Implements the following algorithm:
  // bool sign = (v < 0) == (u < 0);
  // int t = (v < 0 ? -(1 + v) : v) / |u|;
  // return (sign ? t : -(1 + t)) + (u < 0);
  auto v_check = _b.b.CreateICmpSLT(v, zero, "div");
  auto u_check = _b.b.CreateICmpSLT(u, zero, "div");
  auto sign = _b.b.CreateICmpEQ(v_check, u_check, "div");
  auto u_abs = _b.b.CreateSelect(
      u_check, _b.b.CreateSub(zero, u, "div"), u, "div");

  auto t = _b.b.CreateSelect(
      v_check, _b.b.CreateSub(minus_one, v, "div"), v, "div");
  t = _b.b.CreateSDiv(t, u_abs, "div");
  return _b.b.CreateAdd(
      _b.b.CreateSelect(sign, t, _b.b.CreateSub(minus_one, t, "div"), "div"),
      b2i(Value(zero.type, u_check)), "div");
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
  bool is_left = left.type.is_vector();
  std::size_t size = is_left ?
      left.type.get_vector_size() : right.type.get_vector_size();

  Value v = left.type.is_int() || left.type.is_int_vector() ?
      _b.constant_int_vector(0, size) : _b.constant_float_vector(0, size);

  for (std::size_t i = 0; i < size; ++i) {
    v.irval = _b.b.CreateInsertElement(
        v, (is_left ? right : left), _b.constant_int(i), "vec");
  }
  return is_left ? raw_binary(node, left, v) : raw_binary(node, v, right);
}

Value IrGenerator::fold(const Node& node, const Value& value,
                        bool to_bool, bool with_ands, bool right_assoc)
{
  yang::Type etype =
      value.type.is_int_vector() ? yang::Type::int_t() : yang::Type::float_t();

  // Convert each argument to boolean, if necessary.
  std::vector<Value> elements;
  for (std::size_t i = 0; i < value.type.get_vector_size(); ++i) {
    Value v(etype, _b.b.CreateExtractElement(
        value, _b.constant_int(i), "idx"));
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
      v = raw_binary(node, *it, v);
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
    v.irval = _b.b.CreateAnd(v, comparisons[i], "fold");
  }
  return v;
}

llvm::BasicBlock* IrGenerator::create_block(
    metadata meta, const std::string& name)
{
  auto parent = _b.b.GetInsertBlock() ?
      _b.b.GetInsertBlock()->getParent() : nullptr;
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), name, parent);
  _metadata.add(meta, block);
  return block;
}

// End namespace yang::internal.
}
}
