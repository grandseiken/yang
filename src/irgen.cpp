//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "irgen.h"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
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
// TODO: refcounting is super naive at the moment. We increment and decrement
// all over the place even when analysis could show it's unnecessary. Fix that.
// Write benchmarks to show improvement.

IrGenerator::IrGenerator(llvm::Module& module, llvm::ExecutionEngine& engine,
                         StaticData& static_data, const symbol_frame& globals,
                         const ContextInternals& context)
  : IrCommon(module, engine, static_data)
  , _context(context)
  , _chunk(_b)
{
  _scopes.emplace_back(_b);
  // Set up the global data type. Since each program is designed to support
  // multiple independent instances running simultaeneously, the idea is to
  // define a structure type with a field for each global variable. Each
  // function will take a pointer to the global data structure as an implicit
  // final parameter.
  _scopes[0].init_structure_type("global_data", globals, true);
  // Suppose a user type T has a member R(A...) f. So that an expression t.f
  // can be used as a generic function value, it creates an implicit closure;
  // that is, "val = t.f;" is equivalent to:
  // const bind = t; val = R(A... a) {return T::f(bind, a...);};
  // Rather than transforming the AST to do this, we do it manually: use a
  // single closure structure type with just a void pointer, and transform t.f
  // to (mem[T::f], env_mem(t)).
  symbol_frame user_type;
  user_type.emplace("object", yang::Type::user_t<void>(false));
  _chunk.init_structure_type("chunk", user_type, false);

  // We need to generate a reverse trampoline function for each function in the
  // Context. User type member functions are present in the context function map
  // as well as free functions.
  for (const auto& pair : context.functions) {
    get_reverse_trampoline_function(pair.second.type, false);
  }
}

void IrGenerator::emit_global_functions()
{
  auto& scope = _scopes[0];
  // Create allocator function. It takes a pointer to the Yang program instance.
  auto alloc = llvm::Function::Create(
      llvm::FunctionType::get(scope.structure().type, false),
      llvm::Function::ExternalLinkage, "!global_alloc", &_b.module);
  auto alloc_block = llvm::BasicBlock::Create(
      _b.b.getContext(), "entry", alloc);
  _b.b.SetInsertPoint(alloc_block);

  // Call global initialisation functions.
  llvm::Value* v = scope.allocate_structure_value();
  for (llvm::Function* f : _global_inits) {
    _b.b.CreateCall(f, v);
  }
  _b.b.CreateRet(v);

  // Add global destruction functions to custom destructor (in reverse order).
  llvm::Function* custom = scope.structure().custom_destructor;
  auto free_block =
      llvm::BasicBlock::Create(_b.b.getContext(), "entry", custom);
  _b.b.SetInsertPoint(free_block);
  auto it = custom->arg_begin();
  it->setName("struct");
  for (auto jt = _global_destructors.rbegin();
       jt != _global_destructors.rend(); ++jt) {
    _b.b.CreateCall(*jt, it);
  }
  _b.b.CreateRetVoid();

  // Create accessor functions for each field of the global structure.
  for (const auto pair : scope.structure().table) {
    llvm::Type* t = _b.get_llvm_type(pair.second.type);

    std::string name = "!global_get_" + pair.first;
    auto function_type =
        llvm::FunctionType::get(t, scope.structure().type, false);
    auto getter = llvm::Function::Create(
        function_type, llvm::Function::ExternalLinkage, name, &_b.module);
    get_trampoline_function(
        yang::Type::function_t(pair.second.type, {}), false);

    auto getter_block = llvm::BasicBlock::Create(
        _b.b.getContext(), "entry", getter);
    auto it = getter->arg_begin();
    it->setName("global");
    _b.b.SetInsertPoint(getter_block);
    _b.b.CreateRet(scope.memory_load(&*it, pair.first));

    name = "!global_set_" + pair.first;
    std::vector<llvm::Type*> setter_args{t, scope.structure().type};
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
    scope.memory_store(&*it, &*jt, pair.first);
    _b.b.CreateRetVoid();
  }
}

void IrGenerator::obtain_function_pointers()
{
  // Getting function pointers to store in vtables is delayed until after
  // optimisation has taken place, or they will be wrong.
  for (const auto& pair : _b.generated_function_pointers) {
    *pair.first = _b.engine.getPointerToFunction(pair.second);
  }
  _b.generated_function_pointers.clear();
}

void IrGenerator::before(const Node& node)
{
  std::function<Value(const result_list&)> result_macro;
#define RESULT [=,&node](const result_list& results) -> Value
#define RESULT_FOR_ANY(condition) if (condition) result_macro = RESULT
#define RESULT_FOR(node_type) RESULT_FOR_ANY(node.type == Node::node_type)
#define CONSTANT [=,&node](const result_list&) -> Value
#define CONSTANT_FOR_ANY(condition) if (condition) result_macro = CONSTANT
#define CONSTANT_FOR(node_type) CONSTANT_FOR_ANY(node.type == Node::node_type)

  CONSTANT_FOR(TYPE_VOID) {return yang::Type::void_t();};
  CONSTANT_FOR(TYPE_INT) {return node.int_value > 1 ?
      yang::Type::ivec_t(node.int_value) : yang::Type::int_t();};
  CONSTANT_FOR(TYPE_FLOAT) {return node.int_value > 1 ?
      yang::Type::fvec_t(node.int_value) : yang::Type::float_t();};
  RESULT_FOR(NAMED_EXPRESSION) {return results[0];};
  RESULT_FOR(EXPR_STMT) {return results[0];};
  RESULT_FOR(RETURN_STMT) {
    auto dead_block = _scopes.back().create_block("dead");
    _scopes.back().dereference_scoped_locals(0);
    node.children.empty() ? _b.b.CreateRetVoid() : _b.b.CreateRet(results[0]);
    _b.b.SetInsertPoint(dead_block);
    return node.children.empty() ? Value() : results[0];
  };
  RESULT_FOR_ANY(node.type == Node::ASSIGN ||
                 node.type == Node::ASSIGN_LOGICAL_OR ||
                 node.type == Node::ASSIGN_LOGICAL_AND ||
                 node.type == Node::ASSIGN_BITWISE_OR ||
                 node.type == Node::ASSIGN_BITWISE_AND ||
                 node.type == Node::ASSIGN_BITWISE_XOR ||
                 node.type == Node::ASSIGN_BITWISE_LSHIFT ||
                 node.type == Node::ASSIGN_BITWISE_RSHIFT ||
                 node.type == Node::ASSIGN_POW ||
                 node.type == Node::ASSIGN_MOD ||
                 node.type == Node::ASSIGN_ADD ||
                 node.type == Node::ASSIGN_SUB ||
                 node.type == Node::ASSIGN_MUL ||
                 node.type == Node::ASSIGN_DIV) {
    const std::string& s = node.children[0]->string_value;
    Value v = node.type == Node::ASSIGN ?
        results[1] : binary(node, results[0], results[1]);
    _scopes.back().memory_store(v, get_variable_ptr(s));
    return v;
  };
  CONSTANT_FOR_ANY(node.type == Node::BREAK_STMT ||
                   node.type == Node::CONTINUE_STMT) {
    _scopes.back().dereference_loop_locals();
    auto dead_block = _scopes.back().create_block("dead");
    _b.b.CreateBr(_scopes.back().get_block(
        node.type == Node::BREAK_STMT ? LexScope::LOOP_BREAK_LABEL :
                                        LexScope::LOOP_CONTINUE_LABEL));
    _b.b.SetInsertPoint(dead_block);
    return {};
  };
  RESULT_FOR(MEMBER_SELECTION) {
    llvm::Value* env = nullptr;
    // Don't need to allocate anything for managed user-types: we already
    // have a chunk.
    if (results[0].type.is_managed_user_type()) {
      env = results[0];
    }
    else {
      env = _chunk.allocate_closure_struct(get_global_struct());
      _chunk.memory_store(results[0], env, "object");
    }
    Value v = get_member_function(results[0].type, node.string_value);
    v = _b.function_value(v.type, v, env);
    _scopes.back().update_reference_count(v, 1);
    _scopes.back().refcount_init(v);
    return v;
  };
  CONSTANT_FOR(IDENTIFIER) {
    // In type-context, we just want to return a user type.
    if (_scopes.back().metadata.has(LexScope::TYPE_EXPR_CONTEXT)) {
      return _context.type_lookup(node.string_value);
    }

    // Load the variable, if it's there. We must be careful to only return
    // globals/functions *after* they have been defined, otherwise it might
    // be a reference to a context function of the same name.
    Value variable_ptr = get_variable_ptr(node.string_value);
    if (variable_ptr) {
      return variable_ptr.irval->getType()->isPointerTy() ?
          _scopes.back().memory_load(variable_ptr.type, variable_ptr) :
          variable_ptr;
    }

    // It must be a context function.
    const auto& t = _context.constructor_lookup(node.string_value);
    if (!t.ctor.type.is_void()) {
      Value v = get_constructor(node.string_value);
      return _b.function_value(v.type, v, get_global_struct());
    }
    const auto& f = _context.function_lookup(node.string_value);
    if (!f.type.is_void()) {
      return _b.function_value(f);
    }
    // It's possible that nothing matches, when this is the identifier on the
    // left of a variable declaration.
    return Value();
  };
  CONSTANT_FOR(EMPTY_EXPR) {return _b.constant_int(1);};
  CONSTANT_FOR(INT_LITERAL) {return _b.constant_int(node.int_value);};
  CONSTANT_FOR(FLOAT_LITERAL) {return _b.constant_float(node.float_value);};
  CONSTANT_FOR(STRING_LITERAL) {
    StaticString* s = nullptr;
    auto it = _string_literals.find(node.string_value);
    if (it == _string_literals.end()) {
      s = new StaticString(node.string_value);
      _string_literals.emplace(node.string_value, _b.static_data.size());
      _b.static_data.emplace_back(s);
    }
    else {
      s = (StaticString*)_b.static_data[it->second].get();
    }

    // TODO: string literal keep-alives the instance it was spawned from
    // (get_global_struct()), when it really only needs to keep-alive the
    // program.
    llvm::Value* chunk = _chunk.allocate_closure_struct(get_global_struct());
    _chunk.memory_store(_b.constant_ptr(s->value.c_str()), chunk, "object");
    return Value(type_of<Ref<const char>>(),
                 _b.b.CreateBitCast(chunk, _b.void_ptr_type()));
  };
  // Most binary operators map directly to (vectorisations of) LLVM IR
  // instructions.
  RESULT_FOR_ANY(node.type == Node::BITWISE_OR ||
                 node.type == Node::BITWISE_AND ||
                 node.type == Node::BITWISE_XOR ||
                 node.type == Node::BITWISE_LSHIFT ||
                 node.type == Node::BITWISE_RSHIFT ||
                 node.type == Node::POW || node.type == Node::MOD ||
                 node.type == Node::ADD || node.type == Node::SUB ||
                 node.type == Node::MUL || node.type == Node::DIV) {
    return binary(node, results[0], results[1]);
  };
  RESULT_FOR_ANY(node.type == Node::EQ || node.type == Node::NE ||
                 node.type == Node::GE || node.type == Node::LE ||
                 node.type == Node::GT || node.type == Node::LT) {
    return b2i(binary(node, results[0], results[1]));
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_LOGICAL_OR ||
                 node.type == Node::FOLD_LOGICAL_AND) {
    return b2i(fold(node, results[0], true));
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_BITWISE_OR ||
                 node.type == Node::FOLD_BITWISE_OR ||
                 node.type == Node::FOLD_BITWISE_AND ||
                 node.type == Node::FOLD_BITWISE_XOR ||
                 node.type == Node::FOLD_BITWISE_LSHIFT ||
                 node.type == Node::FOLD_BITWISE_RSHIFT) {
    return fold(node, results[0]);
  };
  RESULT_FOR(FOLD_POW) {
    // POW is the only right-associative fold operator.
    return fold(node, results[0], false, false, true);
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_MOD ||
                 node.type == Node::FOLD_ADD || node.type == Node::FOLD_SUB ||
                 node.type == Node::FOLD_MUL || node.type == Node::FOLD_DIV) {
    return fold(node, results[0]);
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_EQ || node.type == Node::FOLD_NE ||
                 node.type == Node::FOLD_GE || node.type == Node::FOLD_LE ||
                 node.type == Node::FOLD_GT || node.type == Node::FOLD_LT) {
    return b2i(fold(node, results[0], false, true));
  };
  RESULT_FOR(LOGICAL_NEGATION) {
    Value v = _b.default_for_type(results[0].type);
    v.irval = _b.b.CreateICmpEQ(results[0], v);
    return b2i(v);
  };
  RESULT_FOR(BITWISE_NEGATION) {
    Value v = _b.default_for_type(results[0].type, 0u - 1);
    v.irval = _b.b.CreateXor(results[0], v);
    return v;
  };
  RESULT_FOR(ARITHMETIC_NEGATION) {
    Value v = _b.default_for_type(results[0].type);
    v.irval = results[0].type.is_int() || results[0].type.is_ivec() ?
        _b.b.CreateSub(v, results[0]) : _b.b.CreateFSub(v, results[0]);
    return v;
  };
  RESULT_FOR_ANY(node.type == Node::INCREMENT || node.type == Node::DECREMENT) {
    Value v = _b.default_for_type(
        results[0].type, node.type == Node::INCREMENT ? 1 : -1);
    v.irval = results[0].type.is_int() || results[0].type.is_ivec() ?
        _b.b.CreateAdd(results[0], v) : _b.b.CreateFAdd(results[0], v);
    if (node.children[0]->type == Node::IDENTIFIER) {
      const std::string& s = node.children[0]->string_value;
      _scopes.back().memory_store(v, get_variable_ptr(s));
    }
    return v;
  };
  RESULT_FOR(INT_CAST) {return f2i(results[0]);};
  RESULT_FOR(FLOAT_CAST) {return i2f(results[0]);};
  RESULT_FOR(VECTOR_CONSTRUCT) {
    Value v = results[0].type.is_int() ? _b.constant_ivec(0, results.size()) :
                                         _b.constant_fvec(0, results.size());
    for (std::size_t i = 0; i < results.size(); ++i) {
      v.irval = _b.b.CreateInsertElement(
          v.irval, results[i], _b.constant_int(i));
    }
    return v;
  };
  RESULT_FOR(VECTOR_INDEX) {
    // Indexing out-of-bounds produces constant zero.
    Value v = results[0].type.is_ivec() ?
        _b.constant_int(0) : _b.constant_float(0);
    auto ge = _b.b.CreateICmpSGE(results[1], _b.constant_int(0));
    auto lt = _b.b.CreateICmpSLT(
        results[1], _b.constant_int(results[0].type.vector_size()));

    auto in = _b.b.CreateAnd(ge, lt);
    v.irval =_b.b.CreateSelect(
        in, _b.b.CreateExtractElement(results[0], results[1]), v);
    return v;
  };
#undef RESULT_FOR
#undef RESULT_FOR_ANY
  if (result_macro) {
    result(node, result_macro);
  }

  switch (node.type) {
    case Node::TYPE_FUNCTION:
      type_function:
      result(node, RESULT {
        std::vector<yang::Type> args;
        for (std::size_t i = 1; i < results.size(); ++i) {
          args.push_back(results[i].type);
        }
        return yang::Type::function_t(results[0].type, args);
      });
      break;

    case Node::GLOBAL:
    {
      // GLOBAL init functions don't need external linkage, since they are
      // called automatically by the externally-visible global structure
      // allocation/deallocation functions.
      auto function = llvm::Function::Create(
          llvm::FunctionType::get(_b.void_type(),
                                  _scopes[0].structure().type, false),
          llvm::Function::InternalLinkage, "!global_init", &_b.module);
      (node.int_value & Node::MODIFIER_NEGATION ?
       _global_destructors : _global_inits).push_back(function);
      auto block =
          llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);

      // Store a special entry in the symbol table for the implicit global
      // structure pointer.
      auto it = function->arg_begin();
      it->setName("env");
      _scopes.emplace_back(_scopes.back().next_lex_scope());
      _b.b.SetInsertPoint(block);

      _scopes.back().metadata.add(LexScope::ENVIRONMENT_PTR, &*it);
      _scopes.back().metadata.add(LexScope::FUNCTION, function);
      if (!node.static_info.closed_environment.empty()) { 
        _scopes.back().init_structure_type(
            "closure", node.static_info.closed_environment, false);

        llvm::Value* v = _scopes.back().allocate_closure_struct(&*it);
        _scopes.back().metadata[LexScope::CLOSURE_PTR] = v;
        _scopes.back().update_reference_count(nullptr, v, 1);
        _scopes.back().refcount_init(_b.function_value(
            yang::Type::function_t(yang::Type::void_t(), {}),
            _b.constant_ptr(nullptr), v));
      }

      if (!(node.int_value & Node::MODIFIER_NEGATION)) {
        _scopes.back().metadata.add(LexScope::GLOBAL_INIT_FUNCTION, function);
      }
      result(node, CONSTANT {
        _scopes.back().dereference_scoped_locals(0);
        _scopes.pop_back();
        _b.b.CreateRetVoid();
        return {};
      });
      break;
    }

    case Node::GLOBAL_ASSIGN:
      if (node.children[1]->type == Node::FUNCTION) {
        _immediate_left_assign = node.children[0]->string_value;
      }
      result(node, RESULT {
        auto function = _b.b.GetInsertBlock()->getParent();
        get_trampoline_function(results[1].type, false);
        // Modifiers such as `export` are stored in int_value for now.
        if (node.int_value & Node::MODIFIER_EXPORT) {
          function->setLinkage(llvm::Function::ExternalLinkage);
        }
        function->setName(node.children[0]->string_value);
        _scopes.back().symbol_table.add(
            node.children[0]->string_value, results[1]);
        return results[1];
      });
      break;

    case Node::FUNCTION:
      _scopes.back().metadata.push();
      _scopes.back().metadata.add(LexScope::TYPE_EXPR_CONTEXT, nullptr);
      call_after_result(*node.children[0], [&](const Value& value)
      {
        _scopes.back().metadata.pop();
        create_function(node, value.type);
      });

      result(node, RESULT {
        if (results[0].type.function_return().is_void()) {
          _scopes.back().dereference_scoped_locals(0);
          _b.b.CreateRetVoid();
        }
        else {
          // In a function that passes the static check, control never reaches
          // this point; but the block must have a terminator.
          _b.b.CreateBr(_b.b.GetInsertBlock());
        }
        auto parent_block = _scopes.back().get_block(LexScope::PARENT_BLOCK);
        _scopes.pop_back();

        auto parent = _b.b.GetInsertBlock()->getParent();
        if (!_scopes.back().metadata.has(LexScope::FUNCTION)) {
          return Value(results[0].type, parent);
        }
        // If this was a nested function, set the insert point back to the last
        // block in the enclosing function and make the proper expression value.
        _b.b.SetInsertPoint(parent_block);
        // If the current function created a closure, we have to pass the new
        // environment pointer (which has a parent pointer to the old one)
        // instead. There is some potential for optimisation if an entire tree
        // of inner functions never creates a closure, where we could instead
        // dereference and pass only the closure scopes they actually access;
        // it's kind of complicated and not really a huge optimisation, though.
        auto closure = _scopes.back().metadata[LexScope::CLOSURE_PTR];
        return _b.function_value(
            results[0].type, parent,
            closure ? closure :
                _scopes.back().metadata[LexScope::ENVIRONMENT_PTR]);
      });
      break;

    case Node::BLOCK:
    case Node::LOOP_AFTER_BLOCK:
      _scopes.back().push_scope();
      result(node, CONSTANT {
        auto after_block = _scopes.back().create_block("after");
        _b.b.CreateBr(after_block);
        _b.b.SetInsertPoint(after_block);
        _scopes.back().pop_scope();
        return {};
      });
      break;

    case Node::IF_STMT:
    {
      _scopes.back().push_scope();
      auto then_block = _scopes.back().create_block("then");
      auto merge_block = _scopes.back().create_block("merge");
      llvm::BasicBlock* else_block = nullptr;
      bool has_else = node.children.size() > 2;
      if (has_else) {
        else_block = _scopes.back().create_block("else");
        call_after(*node.children[1], [=]
        {
          _b.b.CreateBr(merge_block);
          _b.b.SetInsertPoint(else_block);
        });
      }

      call_after_result(*node.children[0], [=](const Value& value)
      {
        _b.b.CreateCondBr(i2b(value), then_block,
                          has_else ? else_block : merge_block);
        _b.b.SetInsertPoint(then_block);
      });
      result(node, RESULT {
        _b.b.CreateBr(merge_block);
        _b.b.SetInsertPoint(merge_block);
        _scopes.back().pop_scope();
        return results[0];
      });
      break;
    }

    case Node::FOR_STMT:
    {
      _scopes.back().push_scope();
      auto cond_block = _scopes.back().create_block("cond");
      auto loop_block = _scopes.back().create_block("loop");
      auto after_block = _scopes.back().create_block("after");
      auto merge_block = _scopes.back().create_block("merge");
      _scopes.back().metadata.add(LexScope::LOOP_BREAK_LABEL, merge_block);
      _scopes.back().metadata.add(LexScope::LOOP_CONTINUE_LABEL, after_block);

      call_after(*node.children[0], [=]
      {
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(cond_block);
        _scopes.back().push_scope(true);
      });
      call_after_result(*node.children[1], [=](const Value& value)
      {
        auto clean_block = _scopes.back().create_block("clean");
        _b.b.CreateCondBr(i2b(value), loop_block, clean_block);
        _b.b.SetInsertPoint(clean_block);
        _scopes.back().dereference_scoped_locals();
        _b.b.CreateBr(merge_block);
        _b.b.SetInsertPoint(after_block);
      });
      call_after(*node.children[2], [=]
      {
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(loop_block);
      });
      result(node, RESULT {
        _scopes.back().pop_scope(true);
        _b.b.CreateBr(after_block);
        _b.b.SetInsertPoint(merge_block);
        _scopes.back().pop_scope();
        return results[0];
      });
      break;
    }

    case Node::WHILE_STMT:
    {
      _scopes.back().push_scope();
      auto cond_block = _scopes.back().create_block("cond");
      auto merge_block = _scopes.back().create_block("merge");
      auto loop_block = _scopes.back().create_block("loop");
      _scopes.back().metadata.add(LexScope::LOOP_BREAK_LABEL, merge_block);
      _scopes.back().metadata.add(LexScope::LOOP_CONTINUE_LABEL, cond_block);

      _b.b.CreateBr(cond_block);
      _b.b.SetInsertPoint(cond_block);
      _scopes.back().push_scope(true);

      call_after_result(*node.children[0], [=](const Value& value)
      {
        auto clean_block = _scopes.back().create_block("clean");
        _b.b.CreateCondBr(i2b(value), loop_block, clean_block);
        _b.b.SetInsertPoint(clean_block);
        _scopes.back().dereference_scoped_locals();
        _b.b.CreateBr(merge_block);
        _b.b.SetInsertPoint(loop_block);
      });
      result(node, RESULT {
        _scopes.back().pop_scope(true);
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(merge_block);
        _scopes.back().pop_scope();
        return results[0];
      });
      break;
    }

    case Node::DO_WHILE_STMT:
    {
      _scopes.back().push_scope(true);
      auto loop_block = _scopes.back().create_block("loop");
      auto cond_block = _scopes.back().create_block("cond");
      auto merge_block = _scopes.back().create_block("merge");
      _scopes.back().metadata.add(LexScope::LOOP_BREAK_LABEL, merge_block);
      _scopes.back().metadata.add(LexScope::LOOP_CONTINUE_LABEL, cond_block);
      _scopes.back().push_scope(true);

      _b.b.CreateBr(loop_block);
      _b.b.SetInsertPoint(loop_block);

      call_after(*node.children[0], [=]
      {
        _scopes.back().pop_scope(true);
        _scopes.back().push_scope();
        _b.b.CreateBr(cond_block);
        _b.b.SetInsertPoint(cond_block);
      });
      result(node, RESULT {
        _scopes.back().pop_scope();
        _scopes.back().pop_scope();
        _b.b.CreateCondBr(i2b(results[1]), loop_block, merge_block);
        _b.b.SetInsertPoint(merge_block);
        return results[0];
      });
      break;
    }

    case Node::TERNARY:
      call_after_result(*node.children[0], [=,&node](const Value& value)
      {
        // Vectorised ternary can't short-circuit.
        if (value.type.is_vector()) {
          result(node, RESULT {
            return Value(
                results[1].type,
                _b.b.CreateSelect(i2b(results[0]), results[1], results[2]));
          });
          return;
        }
        // Blocks and branching are necessary (as opposed to a select
        // instruction) to short-circuit and avoiding evaluating the other path.
        auto then_block = _scopes.back().create_block("then");
        auto else_block = _scopes.back().create_block("else");
        auto merge_block = _scopes.back().create_block("merge");

        _scopes.back().metadata.push();
        _b.b.CreateCondBr(i2b(value), then_block, else_block);
        _b.b.SetInsertPoint(then_block);
        _scopes.back().push_scope();

        call_after(*node.children[1], [=]
        {
          _scopes.back().pop_scope();
          _b.b.CreateBr(merge_block);

          // Save the last block so we can add it as a predecessor.
          _scopes.back().metadata.add(
              LexScope::OTHER_SOURCE_BLOCK, _b.b.GetInsertBlock());
          _b.b.SetInsertPoint(else_block);
          _scopes.back().push_scope();
        });

        result(node, RESULT {
          _scopes.back().pop_scope();
          auto last_then_block =
              _scopes.back().get_block(LexScope::OTHER_SOURCE_BLOCK);
          auto last_else_block = _b.b.GetInsertBlock();
          _scopes.back().metadata.pop();
          _b.b.CreateBr(merge_block);
          _b.b.SetInsertPoint(merge_block);
          auto phi = _b.b.CreatePHI(_b.get_llvm_type(results[1].type), 2);
          phi->addIncoming(results[1], last_then_block);
          phi->addIncoming(results[2], last_else_block);
          return Value(results[1].type, phi);
        });
      });
      break;

    // Logical OR and AND short-circuit. This interacts in a subtle way with
    // vectorisation: we can short-circuit only when the left-hand operand is a
    // primitive. (We could also check and short-circuit if the left-hand
    // operand is an entirely zero or entirely non-zero vector. We currently
    // don't; it seems a bit daft.)
    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
      call_after_result(*node.children[0], [=,&node](const Value& value)
      {
        if (value.type.is_vector()) {
          // Short-circuiting isn't possible.
          result(node, RESULT {
            return binary(node, b2i(i2b(results[0])), b2i(i2b(results[1])));
          });
          return;
        }
        _scopes.back().metadata.push();
        auto source_block = _b.b.GetInsertBlock();
        auto rhs_block = _scopes.back().create_block("rhs");
        auto merge_block = _scopes.back().create_block("merge");

        if (node.type == Node::LOGICAL_OR) {
          _b.b.CreateCondBr(i2b(value), merge_block, rhs_block);
        }
        else {
          _b.b.CreateCondBr(i2b(value), rhs_block, merge_block);
        }
        _b.b.SetInsertPoint(rhs_block);
        _scopes.back().push_scope();

        result(node, RESULT {
          _scopes.back().pop_scope();
          // Update in case we branched again.
          auto last_rhs_block = _b.b.GetInsertBlock();
          _scopes.back().metadata.pop();

          auto rhs = b2i(i2b(results[1]));
          _b.b.CreateBr(merge_block);
          _b.b.SetInsertPoint(merge_block);
          llvm::Type* type = _b.int_type();
          llvm::Value* constant = nullptr;
          if (results[1].type.is_vector()) {
            std::size_t n = results[1].type.vector_size();
            constant = _b.constant_ivec(node.type == Node::LOGICAL_OR, n);
            type = _b.ivec_type(n);
          }
          else {
            constant = _b.constant_int(node.type == Node::LOGICAL_OR);
          }

          auto phi = _b.b.CreatePHI(type, 2);
          phi->addIncoming(constant, source_block);
          phi->addIncoming(rhs, last_rhs_block);
          return Value(results[1].type, phi);
        });
      });

    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
      // See static.cpp for details on the immediate left assign hack.
      if (node.children[1]->type == Node::FUNCTION) {
        _immediate_left_assign = node.children[0]->string_value;
      }
      result(node, RESULT {
        const std::string& s = node.children[0]->string_value;
        // In a global block, rather than allocating anything we simply store
        // into the prepared fields of the global structure. Also enter the
        // symbol now with a null value so we can distinguish it from top-level
        // functions.
        if (_scopes.back().metadata.has(LexScope::GLOBAL_INIT_FUNCTION) &&
            _scopes.back().symbol_table.size() <= 2) {
          _scopes[0].symbol_table.add(s, Value(results[1].type));
          _scopes[0].memory_store(results[1], get_variable_ptr(s));
          return results[1];
        }

        const std::string& unique_name =
            s + "/" + std::to_string(node.static_info.scope_number);

        llvm::Value* storage = nullptr;
        if (_scopes.back().symbol_table.has(unique_name)) {
          storage = _scopes.back().symbol_table[unique_name];
          _scopes.back().value_to_unique_name_map.emplace(storage, unique_name);
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
          _scopes.back().memory_init(entry, storage);
          _scopes.back().refcount_init(Value(results[1].type, storage));
        }

        _scopes.back().memory_store(results[1], storage);
        _scopes.back().symbol_table.add(s, Value(results[1].type, storage));
        return results[1];
      });
      break;

    case Node::CALL:
      if (_scopes.back().metadata.has(LexScope::TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }
      result(node, RESULT {
        std::vector<Value> args;
        for (std::size_t i = 1; i < results.size(); ++i) {
          args.push_back(results[i]);
        }
        Value r = create_call(results[0], args);
        // Reference count the temporary.
        if (r) {
          _scopes.back().update_reference_count(r, 1);
          _scopes.back().refcount_init(r);
        }
        return r;
      });
      break;

    default: {}
  }
#undef RESULT
}

Value IrGenerator::after(const Node&, const result_list&)
{
  return {};
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

llvm::Value* IrGenerator::get_global_struct()
{
  std::size_t steps = 0;
  auto it = _scopes.rbegin();
  for (++it; it != _scopes.rend(); ++it) {
    if (it->structure().type) {
      ++steps;
    }
  }
  return get_parent_struct(
      steps - 1, _scopes.back().metadata[LexScope::ENVIRONMENT_PTR]);
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
    if (it->structure().type) {
      ++steps;
    }
  }

  std::string unique_name = name;
  llvm::Value* struct_ptr = get_parent_struct(
      steps, _scopes.back().metadata[LexScope::ENVIRONMENT_PTR]);
  llvm::Value* cast = _b.b.CreateBitCast(struct_ptr, it->structure().type);

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
      it->symbol_table[name].type, it->structure_ptr(cast, unique_name));
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
  auto& back = _scopes.back();

  // The code for Node::TYPE_FUNCTION in after() ensures it takes an environment
  // pointer.
  back.metadata.add(LexScope::ENVIRONMENT_PTR, &*eptr);
  back.metadata.add(LexScope::FUNCTION, function);
  back.metadata.add(LexScope::PARENT_BLOCK, &*_b.b.GetInsertPoint());

  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", function);
  _b.b.SetInsertPoint(block);

  // Some optimisations may be possible. For example, const variables may not
  // need to go in the closure structure. Maybe LLVM can handle that anyway?
  //
  // Also, we naively allocate at most one closure structure per function
  // invocation. Obviously, if we could partition the inner functions such that
  // the sets of enclosing variables they access are disjoint, we could allocate
  // separate structures for each (and potentially return unused memory sooner).
  if (!node.static_info.closed_environment.empty()) {
    back.init_structure_type(
        "closure", node.static_info.closed_environment, false);

    llvm::Value* v = back.allocate_closure_struct(&*eptr);
    back.metadata[LexScope::CLOSURE_PTR] = v;
    // Refcounting on closure pointer.
    back.update_reference_count(nullptr, v, 1);
    back.refcount_init(_b.function_value(
        yang::Type::function_t(yang::Type::void_t(), {}),
        _b.constant_ptr(nullptr), v));
  }

  // Lambda for deciding whether to put an argument in closure or stack. These
  // are refcount-decremented on return statements (or end-of-function for void
  // returns).
  auto assign_storage = [&](
      const yang::Type& type, const std::string& name, std::int32_t scope_mod)
  {
    std::string unique_name =
        name + "/" + std::to_string(node.static_info.scope_number + scope_mod);

    if (back.symbol_table.has(unique_name)) {
      llvm::Value* v = back.symbol_table[unique_name];
      back.value_to_unique_name_map.emplace(v, unique_name);
      return v;
    }
    // Rather than reference argument values directly, we create an alloca
    // and store the argument in there. This simplifies things, since we
    // can emit the same IR code when referencing local variables or
    // function arguments.
    llvm::Value* v = _b.b.CreateAlloca(_b.get_llvm_type(type), nullptr);
    back.refcount_init(Value(type, v));
    back.memory_init(_b.b, v);
    return v;
  };

  // Store the function's name in its own scope. Recursive lookup handled
  // similarly to arguments below.
  if (_immediate_left_assign.length()) {
    function->setName(_immediate_left_assign);
    // The identifier is registered one scope above the function argument scope.
    llvm::Value* storage =
        assign_storage(function_type, _immediate_left_assign, -1);
    back.memory_store(
        _b.function_value(function_type, function, eptr), storage);
    back.symbol_table.add(
        _immediate_left_assign, Value(function_type, storage));
    _immediate_left_assign.clear();
  }
  back.symbol_table.push();

  // Set up the arguments.
  std::size_t arg_num = 0;
  for (auto it = function->arg_begin();
       it != --function->arg_end(); ++it, ++arg_num) {
    const std::string& name =
        node.children[0]->children[1 + arg_num]->string_value;
    const yang::Type& arg = function_type.function_arg(arg_num);
    llvm::Value* storage = assign_storage(arg, name, 0);
    it->setName(name);

    // It's possible refcounting isn't necessary on arguments, since they're
    // const and will usually be referenced somewhere up the call stack. I'm not
    // convinced, though.
    back.memory_store(Value(arg, &*it), storage);
    back.symbol_table.add(name, Value(arg, storage));
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

Value IrGenerator::get_member_function(
    const yang::Type& type, const std::string& name)
{
  auto jt = _member_functions[type].find(name);
  if (jt != _member_functions[type].end()) {
    return jt->second;
  }

  const auto& cf = _context.member_lookup(type, name);
  std::vector<yang::Type> args;
  for (std::size_t i = 1; i < cf.type.function_num_args(); ++i) {
    args.push_back(cf.type.function_arg(i));
  }
  yang::Type closed_type =
      yang::Type::function_t(cf.type.function_return(), args);

  auto llvm_type = _b.raw_function_type(closed_type);
  auto f = llvm::Function::Create(
      llvm_type, llvm::Function::InternalLinkage, name, &_b.module);

  llvm::Value* closure = --f->arg_end();
  closure->setName("boxed_object");
  llvm::BasicBlock* prev = _b.b.GetInsertBlock();
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", f);
  _b.b.SetInsertPoint(block);

  std::vector<Value> fargs;
  if (type.is_managed_user_type()) {
    fargs.emplace_back(cf.type.function_arg(0), closure);
  }
  else {
    closure = _b.b.CreateBitCast(closure, _chunk.structure().type);
    auto object = _chunk.memory_load(closure, "object");
    fargs.emplace_back(cf.type.function_arg(0), object);
  }
  std::size_t i = 0;
  for (auto it = f->arg_begin(); it != --f->arg_end(); ++it) {
    fargs.emplace_back(cf.type.function_arg(++i), it);
  }
  _b.b.CreateRet(create_call(_b.function_value(cf), fargs));
  _b.b.SetInsertPoint(prev);

  Value v(closed_type, f);
  _member_functions[type].emplace(name, v);
  return v;
}

Value IrGenerator::get_constructor(const std::string& type)
{
  auto it = _constructors.find(type);
  if (it != _constructors.end()) {
    return it->second;
  }

  // Create the destructor which calls the user-type destructor (and usual chunk
  // destructor).
  const auto& ct = _context.constructor_lookup(type);
  auto destructor_type = (llvm::FunctionType*)
      _chunk.structure().destructor->getType()->getPointerElementType();
  auto destructor = llvm::Function::Create(
      destructor_type, llvm::Function::ExternalLinkage, "~" + type, &_b.module);

  llvm::BasicBlock* prev = _b.b.GetInsertBlock();
  auto block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", destructor);
  _b.b.SetInsertPoint(block);

  llvm::Value* closure = destructor->arg_begin();
  closure->setName("boxed_object");
  closure = _b.b.CreateBitCast(closure, _chunk.structure().type);
  auto object = _chunk.memory_load(closure, "object");
  create_call(_b.function_value(ct.dtor), std::vector<Value>{object});
  _b.b.CreateCall(_chunk.structure().destructor, closure);
  _b.b.CreateRetVoid();

  // Create the user-type vtable.
  Vtable* vtable = _b.create_vtable(
      destructor, _chunk.structure().vtable->refout_count,
      _chunk.structure().refout_query);

  // Create the constructor (which overwrites the usual vtable).
  auto llvm_type = _b.raw_function_type(ct.ctor.type);
  auto f = llvm::Function::Create(
      llvm_type, llvm::Function::InternalLinkage, type, &_b.module);

  block = llvm::BasicBlock::Create(_b.b.getContext(), "entry", f);
  _b.b.SetInsertPoint(block);

  std::vector<Value> fargs;
  std::size_t i = 0;
  for (auto it = f->arg_begin(); it != --f->arg_end(); ++it) {
    fargs.emplace_back(ct.ctor.type.function_arg(++i), it);
  }
  Value r = create_call(_b.function_value(ct.ctor), fargs);
  llvm::Value* global = --f->arg_end();
  llvm::Value* chunk = _chunk.allocate_closure_struct(global);
  _chunk.memory_store(r, chunk, "object");
  _chunk.memory_store(Value(yang::Type::void_t(), _b.constant_ptr(vtable)),
                      _chunk.structure_ptr(chunk, Structure::VTABLE_PTR));
  _b.b.CreateRet(_b.b.CreateBitCast(chunk, _b.void_ptr_type()));
  _b.b.SetInsertPoint(prev);

  // Convert signature to return managed type.
  std::vector<yang::Type> args;
  for (std::size_t i = 0; i < ct.ctor.type.function_num_args(); ++i) {
    args.push_back(ct.ctor.type.function_arg(i));
  }
  const auto& ref_type = ct.ctor.type.function_return();
  Value v(yang::Type::function_t(ref_type.make_managed(true), args), f);
  _constructors.emplace(type, v);
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
  auto cpp_block = _scopes.back().create_block("cpp");
  auto yang_block = _scopes.back().create_block("yang");
  auto merge_block = _scopes.back().create_block("merge");

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
  const yang::Type& return_t = f.type.function_return();
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
    std::size_t size = v.type.vector_size();
    return Value(yang::Type::ivec_t(size),
                 _b.b.CreateZExt(v, _b.ivec_type(size)));
  }
  return Value(yang::Type::int_t(), _b.b.CreateZExt(v, _b.int_type()));
}

Value IrGenerator::i2f(const Value& v)
{
  if (v.type.is_vector()) {
    std::size_t size = v.type.vector_size();
    return Value(yang::Type::fvec_t(size),
                 _b.b.CreateSIToFP(v, _b.fvec_type(size)));
  }
  return Value(yang::Type::float_t(), _b.b.CreateSIToFP(v, _b.float_type()));
}

Value IrGenerator::f2i(const Value& v)
{
  yang::Type type = v.type.is_vector() ?
      yang::Type::ivec_t(v.type.vector_size()) : yang::Type::int_t();
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
      node.type == Node::ASSIGN_LOGICAL_OR ? Node::LOGICAL_OR :
      node.type == Node::ASSIGN_LOGICAL_AND ? Node::LOGICAL_AND :
      node.type == Node::ASSIGN_BITWISE_OR ? Node::BITWISE_OR :
      node.type == Node::ASSIGN_BITWISE_AND ? Node::BITWISE_AND :
      node.type == Node::ASSIGN_BITWISE_XOR ? Node::BITWISE_XOR :
      node.type == Node::ASSIGN_BITWISE_LSHIFT ? Node::BITWISE_LSHIFT :
      node.type == Node::ASSIGN_BITWISE_RSHIFT ? Node::BITWISE_RSHIFT :
      node.type == Node::ASSIGN_POW ? Node::POW :
      node.type == Node::ASSIGN_MOD ? Node::MOD :
      node.type == Node::ASSIGN_ADD ? Node::ADD :
      node.type == Node::ASSIGN_SUB ? Node::SUB :
      node.type == Node::ASSIGN_MUL ? Node::MUL :
      node.type == Node::ASSIGN_DIV ? Node::DIV :
      node.type;

  if (v.type.is_int() or v.type.is_ivec()) {
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

  std::size_t size = v.type.vector_size();
  Value result = to_float ?
      _b.constant_fvec(0, size) : _b.default_for_type(v.type);
  for (std::size_t i = 0; i < size; ++i) {
    std::vector<llvm::Value*> args{
        flt(_b.b.CreateExtractElement(v, _b.constant_int(i))),
        flt(_b.b.CreateExtractElement(u, _b.constant_int(i)))};
    llvm::Value* call = _b.b.CreateCall(f, args);
    result.irval = _b.b.CreateInsertElement(result, call, _b.constant_int(i));
  }
  return to_float && v.type.is_ivec() ? f2i(result) : result;
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
  if (!v.type.is_int() && !v.type.is_ivec()) {
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
  if (!v.type.is_int() && !v.type.is_ivec()) {
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

  for (std::size_t i = 0; i < vector.type.vector_size(); ++i) {
    v.irval = _b.b.CreateInsertElement(v, single, _b.constant_int(i));
  }
  return left.type.is_vector() ?
      raw_binary(node, vector, v) : raw_binary(node, v, vector);
}

Value IrGenerator::fold(const Node& node, const Value& value,
                        bool to_bool, bool with_ands, bool right_assoc)
{
  yang::Type etype =
      value.type.is_ivec() ? yang::Type::int_t() : yang::Type::float_t();

  // Convert each argument to boolean, if necessary.
  std::vector<Value> elements;
  for (std::size_t i = 0; i < value.type.vector_size(); ++i) {
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
