//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "static.h"

#include <unordered_set>
#include <vector>
#include <yang/context.h>

namespace std {
  template<>
  struct hash<yang::internal::StaticChecker::metadata_t> {
    std::size_t operator()(yang::internal::StaticChecker::metadata_t v) const
    {
      return v;
    }
  };
}

namespace yang {
namespace internal {
namespace {

bool is_assignment(const Node& node)
{
  return
      node.type == Node::ASSIGN ||
      node.type == Node::ASSIGN_LOGICAL_OR ||
      node.type == Node::ASSIGN_LOGICAL_AND ||
      node.type == Node::ASSIGN_BITWISE_OR ||
      node.type == Node::ASSIGN_BITWISE_AND ||
      node.type == Node::ASSIGN_BITWISE_LSHIFT ||
      node.type == Node::ASSIGN_BITWISE_RSHIFT ||
      node.type == Node::ASSIGN_POW ||
      node.type == Node::ASSIGN_MOD ||
      node.type == Node::ASSIGN_ADD ||
      node.type == Node::ASSIGN_SUB ||
      node.type == Node::ASSIGN_MUL ||
      node.type == Node::ASSIGN_DIV;
}

bool is_tree_root(const Node& node)
{
  return node.parent && node.get_parent_index() == 0 &&
      (node.parent->type == Node::EXPR_STMT ||
       node.parent->type == Node::LOOP_AFTER_BLOCK ||
       node.parent->type == Node::FOR_STMT);
}

bool is_type_expression(const Node& node)
{
  return
      node.type == Node::TYPE_VOID || node.type == Node::TYPE_INT ||
      node.type == Node::TYPE_FLOAT || node.type == Node::TYPE_FUNCTION;
}

bool valid_all_contexts(const Node& node)
{
  // IDENTIFIER, NAMED_EXPRESSION, and CALL are currently the only things that
  // make sense in both contexts.
  return
      node.type == Node::IDENTIFIER || node.type == Node::NAMED_EXPRESSION ||
      node.type == Node::CALL;
}

bool use_function_immediate_assign_hack(const Node& node)
{
  // Node should be type GLOBAL_ASSIGN, ASSIGN_VAR or ASSIGN_CONST.
  return node.children[0]->type == Node::IDENTIFIER &&
      node.children[1]->type == Node::FUNCTION;
}

const Node* get_no_effect_node(const Node& node)
{
  if (node.type == Node::EMPTY_EXPR || node.type == Node::CALL ||
      node.type == Node::ASSIGN_VAR || node.type == Node::ASSIGN_CONST ||
      node.type == Node::INCREMENT || node.type == Node::DECREMENT ||
      node.type == Node::ASSIGN ||
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
      node.type == Node::ASSIGN_DIV ||
      // Identifier doesn't actually have any effect, but we want probably want
      // to allow its use to disable "unused" warnings. This does mean that e.g.
      // "0 || a;" or "(a, a);" don't warn either, though. Maybe it shouldn't be
      // allowed?
      node.type == Node::IDENTIFIER) {
    return nullptr;
  }
  if (node.type == Node::LOGICAL_OR || node.type == Node::LOGICAL_AND) {
    return get_no_effect_node(*node.children[1]);
  }
  if (node.type == Node::VECTOR_CONSTRUCT) {
    // Without a genuine comma operator, (a(), b()) really does have an
    // effect...
    for (const auto& n : node.children) {
      const Node* m = get_no_effect_node(*n);
      if (m) {
        return m;
      }
    }
    return nullptr;
  }
  if (node.type == Node::TERNARY && (!get_no_effect_node(*node.children[1]) ||
                                     !get_no_effect_node(*node.children[2]))) {
    return nullptr;
  }
  return &node;
}

Type element_type(const Type& t)
{
  return t.is_error() ? Type(true) :
      t.is_int() ? yang::Type::int_t() :
      t.is_float() ? yang::Type::float_t() : Type(true);
}

Type numeric_type(bool is_float, std::size_t num)
{
  return is_float ?
      (num == 1 ? yang::Type::float_t() : yang::Type::fvec_t(num)) :
      (num == 1 ? yang::Type::int_t() : yang::Type::ivec_t(num));
}

Type binary_type(const Type& a, const Type& b, bool is_float)
{
  if (a.is_error() || b.is_error()) {
    return Type(true);
  }
  std::size_t max = std::max(a.external().vector_size(),
                             b.external().vector_size());
  return numeric_type(is_float, max);
}

// End anonymous namespace.
}

StaticChecker::StaticChecker(
    const ContextInternals& context, ParseData& data,
    symbol_frame& functions_output, symbol_frame& globals_output)
  : _context(context)
  , _data(data)
  , _functions_output(functions_output)
  , _globals_output(globals_output)
{
  _scopes.emplace_back(*(Node*)nullptr, "");
}

StaticChecker::~StaticChecker()
{
  // Keep hash<metadata> in source file.
}

void StaticChecker::before(const Node& node)
{
  // Reject type-expressions in non-type contexts and expressions in
  // type-contexts.
  bool context_err = !valid_all_contexts(node) &&
      inside_type_context() != is_type_expression(node);
  if (context_err) {
    if (!_scopes.back().metadata.has(ERR_EXPR_CONTEXT)) {
      if (inside_type_context()) {
        error(node, "expected type in this context");
      }
      else {
        error(node, "unexpected type in this context");
      }
    }
    // Avoid duplicated errors by adding an override context.
    _scopes.back().metadata.push();
    _scopes.back().metadata.add(ERR_EXPR_CONTEXT, {});
    call_after(node, [&]{_scopes.back().metadata.pop();});
  }
  const Node* no_effect_node = get_no_effect_node(node);
  if (is_tree_root(node) && no_effect_node) {
    error(*no_effect_node, "operation has no effect", false);
  }

  // To make the error messages useful, the general idea here is to fall back to
  // the ERROR type only when the operands (up to errors) do not uniquely
  // determine some result type.
  // For example, the erroneous (1, 1) + (1, 1, 1) results in an ERROR type,
  // since there's no way to decide if an int2 or int3 was intended. However,
  // the erroneous 1 == 1. gives type INT, as the result would be INT whether or
  // not the operand type was intended to be int or float.
  std::function<Type(const result_list&)> result_macro;
#define RESULT [&](const result_list& results) -> Type
#define RESULT_FOR_ANY(condition) if (condition) result_macro = RESULT
#define RESULT_FOR(node_type) RESULT_FOR_ANY(node.type == Node::node_type)

  // For simple context-insensitive nodes we define the behaviour with macros.
  RESULT_FOR(TYPE_VOID) {return {};};
  RESULT_FOR(TYPE_INT) {return numeric_type(false, node.int_value);};
  RESULT_FOR(TYPE_FLOAT) {return numeric_type(true, node.int_value);};
  RESULT_FOR(NAMED_EXPRESSION) {return results[0];};
  RESULT_FOR(EXPR_STMT) {return Type(true);};
  RESULT_FOR(RETURN_STMT) {
    Type t = node.children.empty() ? Type() : results[0];
    // If we're not in a function, we must be in a global block.
    if (!inside_function()) {
      error(node, "return statement inside `global`");
      return t;
    }
    if (!t.is(current_return_type())) {
      const auto& n = node.children.empty() ? node : *node.children[0];
      error(n, "returning " + str(t) + " from " +
               str(current_return_type()) + " function");
    }
    return current_return_type();
  };
  RESULT_FOR(MEMBER_SELECTION) {
    if (!results[0].user_type()) {
      error(node, "member function access on " + str(results[0]));
      return Type(true);
    }
    if (results[0].is_error()) {
      return Type(true);
    }

    const auto& m =
        _context.member_lookup(results[0].external(), node.string_value);
    if (m.type.is_void()) {
      error(node, "undeclared member function `" +
                  results[0].string(_context, false) +
                  "::" + node.string_value + "`");
      return Type(true);
    }
    // Omit the first argument (self). Unfortunately, the indirection here
    // makes errors when calling the returned function somewhat vague.
    std::vector<yang::Type> args;
    for (std::size_t i = 1; i < m.type.function_num_args(); ++i) {
      args.push_back(m.type.function_arg(i));
    }
    return yang::Type::function_t(m.type.function_return(), args);
  };
  RESULT_FOR_ANY(node.type == Node::BREAK_STMT ||
                 node.type == Node::CONTINUE_STMT) {
    if (!_scopes.back().metadata.has(LOOP_BODY)) {
      error(node, str(node) + " outside of loop body");
    }
    return Type(true);
  };
  RESULT_FOR(IDENTIFIER) {
    // Look up user types in a type-context.
    if (inside_type_context()) {
      const auto& t = _context.type_lookup(node.string_value);
      if (!t.is_void()) {
        return t;
      }
      error(node, "undeclared type `" + node.string_value + "`");
      return Type(true);
    }

    // Regular symbols.
    auto it = _scopes.rbegin();
    for (; it != _scopes.rend(); ++it) {
      if (it->symbol_table.has(node.string_value)) {
        break;
      }
      if (it == --_scopes.rend()) {
        // Check Context if symbol isn't present in the Program table.
        const auto& t = _context.constructor_lookup(node.string_value);
        if (!t.ctor.type.is_void()) {
          // Fix the constructor return type.
          std::vector<yang::Type> args;
          for (std::size_t i = 0;
               i < t.ctor.type.function_num_args(); ++i) {
            args.push_back(t.ctor.type.function_arg(i));
          }
          return yang::Type::function_t(
              t.ctor.type.function_return().make_managed(true), args);
        }

        const auto& f = _context.function_lookup(node.string_value);
        if (!f.type.is_void()) {
          return f.type;
        }

        error(node, "undeclared identifier `" + node.string_value + "`");
        add_symbol(node, node.string_value, Type(true), false, false);
        it = _scopes.rbegin();
        break;
      }
    }

    auto& symbol = it->symbol_table[node.string_value];
    // If this is a reference to variable in an enclosing function, make
    // sure it's added to that function's closed environment.
    if (it != _scopes.rbegin() && it != --_scopes.rend()) {
      if (!symbol.closed) {
        error(node, "symbol `" + node.string_value + "` declared without "
                    "`closed` modifier in enclosing function");
        symbol.closed = true;
      }
      symbol.warn_closed = false;
      it->function.static_info.closed_environment.emplace(
          node.string_value + "/" + std::to_string(symbol.scope_number),
          symbol.type.external());
    }

    // Update read/write warnings.
    (is_assignment(*node.parent) && node.get_parent_index() == 0 ?
         symbol.warn_writes : symbol.warn_reads) = false;
    return symbol.type;
  };
  RESULT_FOR(EMPTY_EXPR) {return yang::Type::int_t();};
  RESULT_FOR(INT_LITERAL) {return yang::Type::int_t();};
  RESULT_FOR(FLOAT_LITERAL) {return yang::Type::float_t();};
  RESULT_FOR(STRING_LITERAL) {return type_of<Ref<const char>>();};
  RESULT_FOR_ANY(node.type == Node::POW || node.type == Node::MOD ||
                 node.type == Node::ADD || node.type == Node::SUB ||
                 node.type == Node::MUL || node.type == Node::DIV) {
    // Takes two integers or floats and produces a value of the same type,
    // with vectorisation.
    if (!results[0].is_binary_match(results[1]) ||
        (!(results[0].is_int() && results[1].is_int()) &&
         !(results[0].is_float() && results[1].is_float()))) {
      error(node, str(node) + " applied to " +
                  str(results[0]) + " and " + str(results[1]));
      return Type(true);
    }
    return binary_type(results[0], results[1], results[0].is_float());
  };
  RESULT_FOR_ANY(node.type == Node::EQ || node.type == Node::NE ||
                 node.type == Node::GE || node.type == Node::LE ||
                 node.type == Node::GT || node.type == Node::LT) {
    // Takes two integers or floats and produces an integer, with
    // vectorisation.
    if (!results[0].is_binary_match(results[1])) {
      error(node, str(node) + " applied to " +
                  str(results[0]) + " and " + str(results[1]));
      return Type(true);
    }
    else if (!(results[0].is_int() && results[1].is_int()) &&
             !(results[0].is_float() && results[1].is_float())) {
      error(node, str(node) + " applied to " +
                  str(results[0]) + " and " + str(results[1]));
    }
    return binary_type(results[0], results[1], false);
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_LOGICAL_OR ||
                 node.type == Node::FOLD_LOGICAL_AND ||
                 node.type == Node::FOLD_BITWISE_OR ||
                 node.type == Node::FOLD_BITWISE_AND ||
                 node.type == Node::FOLD_BITWISE_XOR ||
                 node.type == Node::FOLD_BITWISE_LSHIFT ||
                 node.type == Node::FOLD_BITWISE_RSHIFT) {
    if (!results[0].is_vector() || !results[0].is_int()) {
      error(node, str(node) + " applied to " + str(results[0]));
    }
    return yang::Type::int_t();
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_POW || node.type == Node::FOLD_MOD ||
                 node.type == Node::FOLD_ADD || node.type == Node::FOLD_SUB ||
                 node.type == Node::FOLD_MUL || node.type == Node::FOLD_DIV) {
    if (!results[0].is_vector() ||
        !(results[0].is_int() || results[0].is_float())) {
      error(node, str(node) + " applied to " + str(results[0]));
      return Type(true);
    }
    return element_type(results[0]);
  };
  RESULT_FOR_ANY(node.type == Node::FOLD_EQ || node.type == Node::FOLD_NE ||
                 node.type == Node::FOLD_GE || node.type == Node::FOLD_LE ||
                 node.type == Node::FOLD_GT || node.type == Node::FOLD_LT) {
    if (!results[0].is_vector() ||
        !(results[0].is_int() || results[0].is_float())) {
      error(node, str(node) + " applied to " + str(results[0]));
    }
    return yang::Type::int_t();
  };
  RESULT_FOR_ANY(node.type == Node::LOGICAL_NEGATION ||
                 node.type == Node::BITWISE_NEGATION) {
    if (!results[0].is_int()) {
      error(node, str(node) + " applied to " + str(results[0]));
    }
    return numeric_type(false, results[0].external().vector_size());
  };
  RESULT_FOR(ARITHMETIC_NEGATION) {
    if (!(results[0].is_int() || results[0].is_float())) {
      error(node, str(node) + " applied to " + str(results[0]));
      return Type(true);
    }
    return results[0];
  };
  RESULT_FOR_ANY(node.type == Node::INCREMENT || node.type == Node::DECREMENT) {
    if (!(results[0].is_int() || results[0].is_float())) {
      error(node, str(node) + " applied to " + str(results[0]));
      return Type(true);
    }
    Type t = results[0];
    if (node.children[0]->type == Node::IDENTIFIER) {
      if (results[0].external().is_const()) {
        error(node, str(node) + " applied to " + str(results[0]));
        t = t.make_const(false);
      }
      std::string s = node.children[0]->string_value;
      for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it) {
        if (it->symbol_table.has(s)) {
          it->symbol_table[s].warn_writes = false;
        }
      }
    }
    return t;
  };
  RESULT_FOR_ANY(
      node.type == Node::ASSIGN ||
      node.type == Node::ASSIGN_LOGICAL_OR ||
      node.type == Node::ASSIGN_LOGICAL_AND ||
      node.type == Node::ASSIGN_BITWISE_OR ||
      node.type == Node::ASSIGN_BITWISE_AND ||
      node.type == Node::ASSIGN_BITWISE_XOR ||
      node.type == Node::ASSIGN_BITWISE_LSHIFT ||
      node.type == Node::ASSIGN_BITWISE_RSHIFT ||
      node.type == Node::ASSIGN_POW || node.type == Node::ASSIGN_MOD ||
      node.type == Node::ASSIGN_ADD || node.type == Node::ASSIGN_SUB ||
      node.type == Node::ASSIGN_MUL || node.type == Node::ASSIGN_DIV) {
    if (node.children[0]->type != Node::IDENTIFIER) {
      if (!results[0].is_error()) {
        error(node, "assignments must be directly to an identifier");
      }
      return results[1];
    }

    const std::string& ident = node.children[0]->string_value;
    for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it) {
      if (!it->symbol_table.has(ident)) {
        continue;
      }
      Type t = it->symbol_table[ident].type;

      if (!is_tree_root(node)) {
        it->symbol_table[ident].warn_reads = false;
      }

      if (node.type != Node::ASSIGN) {
        bool math =
            node.type == Node::ASSIGN_POW ||
            node.type == Node::ASSIGN_MOD ||
            node.type == Node::ASSIGN_ADD ||
            node.type == Node::ASSIGN_SUB ||
            node.type == Node::ASSIGN_MUL ||
            node.type == Node::ASSIGN_DIV;

        if (!t.is_assign_binary_match(results[1])) {
          error(node, str(node) + " applied to " +
                      str(t) + " and " + str(results[1]));
          return Type(true);
        }
        if (!math && (!t.is_int() || !results[1].is_int())) {
          error(node, str(node) + " applied to " +
                      str(t) + " and " + str(results[1]));
          return binary_type(t, results[1], false);
        }
        if (math && !(t.is_int() && results[1].is_int()) &&
            !(t.is_float() && results[1].is_float())) {
          error(node, str(node) + " applied to " +
                      str(t) + " and " + str(results[1]));
          return binary_type(t, results[1], results[0].is_float());
        }
      }
      if (node.type == Node::ASSIGN && !t.is(results[1])) {
        error(node, str(results[1]) + " assigned to `" + ident +
                    "` of type " + str(t));
        return Type(true);
      }
      if (t.external().is_const()) {
        error(node, "assignment to `" + ident + "` of type " + str(t));
      }
      return t;
    }

    if (!_context.function_lookup(ident).type.is_void()) {
      error(node, "cannot assign to context function `" + ident + "`");
    }
    else if (!_context.constructor_lookup(ident).ctor.type.is_void()) {
      error(node, "cannot assign to context constructor `" + ident + "`");
    }
    else if (!_context.type_lookup(ident).is_void()) {
      error(node, "cannot assign to context type `" + ident + "`");
    }
    else {
      error(node, "undeclared identifier `" + ident + "`");
    }
    add_symbol(node, ident, results[1], false, false);
    return results[1];
  };
  RESULT_FOR(INT_CAST) {
    if (!results[0].is_float()) {
      error(node, str(node) + " applied to " + str(results[0]));
    }
    return numeric_type(false, results[0].external().vector_size());
  };
  RESULT_FOR(FLOAT_CAST) {
    if (!results[0].is_int()) {
      error(node, str(node) + " applied to " + str(results[0]));
    }
    return numeric_type(true, results[0].external().vector_size());
  };
  RESULT_FOR(VECTOR_CONSTRUCT) {
    Type t = results[0];
    std::string ts;
    bool unify_error = false;
    std::unordered_set<Type> bad_types;
    for (std::size_t i = 0; i < results.size(); ++i) {
      if (node.children[i]->type == Node::NAMED_EXPRESSION) {
        error(*node.children[i],
              str(node) + ": named argument in vector construction");
      }
      if (!results[i].primitive() && !bad_types.count(results[i])) {
        // Store the bad types we saw already so as not to repeat the error.
        error(*node.children[i],
              str(node) + ": element with non-primitive type " +
              str(results[i]) + " in vector construction");
        t = Type(true);
        bad_types.insert(results[i]);
      }
      if (i) {
        bool error = t.is_error();
        t = t.unify(results[i]);
        if (!error && t.is_error()) {
          unify_error = true;
        }
        ts += ", ";
      }
      ts += str(results[i]);
    }
    if (unify_error) {
      error(node, str(node) + " applied to different types " + ts);
      return Type(true);
    }
    return numeric_type(t.is_float(), results.size());
  };
  RESULT_FOR(VECTOR_INDEX) {
    if (!results[0].is_vector() || !results[1].is(yang::Type::int_t())) {
      error(node, str(node) + " applied to " +
                  str(results[0]) + " and " + str(results[1]));
      return results[0].is_vector() ? element_type(results[0]) : Type(true);
    }
    return element_type(results[0]);
  };

#undef RESULT_FOR
#undef RESULT_FOR_ANY
  if (result_macro) {
    result(node, result_macro);
  }

  switch (node.type) {
    case Node::TYPE_FUNCTION:
      type_function:
      result(node, RESULT
      {
        bool err = results[0].is_error();
        std::vector<yang::Type> args;
        for (std::size_t i = 1; i < results.size(); ++i) {
          if (!results[i].not_void()) {
            error(*node.children[i], "function type with `void` argument type");
          }
          if (results[i].is_error()) {
            err = true;
          }
          args.push_back(results[i].external());
        }
        return err ? Type(true) :
            yang::Type::function_t(results[0].external(), args);
      });
      break;

    case Node::PROGRAM:
      // Make sure to warn on unused top-level elements. This doesn't actually
      // pop anything, since it's the last frame.
      call_after(node, [&]{pop_symbol_tables();});
      result(node, RESULT {return Type{};});
      break;

    case Node::GLOBAL:
      if (node.int_value & Node::MODIFIER_NEGATION) {
        _scopes.emplace_back(node, "<~global>");
        _scopes.back().metadata.add(GLOBAL_DESTRUCTOR, {});
      }
      else {
        _scopes.emplace_back(node, "<global>");
      }
      push_symbol_tables();
      if (node.int_value & Node::MODIFIER_EXPORT) {
        if (node.int_value & Node::MODIFIER_NEGATION) {
          error(node, "~global marked export");
        }
        else {
          _scopes.back().metadata.add(EXPORT_GLOBAL, {});
        }
      }
      call_after(node, [&]
      {
        pop_symbol_tables();
        _scopes.pop_back();
      });
      result(node, RESULT {return Type{};});
      break;

    case Node::GLOBAL_ASSIGN:
    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
      // Super big hack: in order to allow recursion, the function has to have
      // a name already in the scope of its body. This doesn't really make much
      // sense with function-expressions! But, we can use a hack where, if a
      // function-expression appears on the immediate right-hand side of an
      // assignment, we make a note and store it in the symbol table early.
      //
      // We don't allow different functions to be mutually recursive, since that
      // necessarily requires a two-phase approach. However, it's not all that
      // important and can be achieved for any particular pair of functions by
      // nesting, anyway.
      if (use_function_immediate_assign_hack(node)) {
        _immediate_left_assign = node.children[0]->string_value;
      }
      // Add the identifier to the table temporarily to avoid lookup errors on
      // the name.
      push_symbol_tables();
      call_after(*node.children[0], [&]{pop_symbol_tables();});
      if (node.children[0]->type == Node::IDENTIFIER) {
        add_symbol(node, node.children[0]->string_value, {}, false, false);
      }

      result(node, RESULT {
        Type t = results[1].make_const(node.type == Node::ASSIGN_CONST ||
                                       node.type == Node::GLOBAL_ASSIGN);

        if (node.type == Node::GLOBAL_ASSIGN && !results[1].function()) {
          error(node, "global assignment of type " + str(results[1]));
          t = Type(true);
        }
        if (node.type != Node::GLOBAL_ASSIGN && !results[1].not_void()) {
          error(node, "assignment of type " + str(results[1]));
          t = Type(true);
        }
        if (node.children[0]->type != Node::IDENTIFIER) {
          error(node, "assignments must be directly to an identifier");
          return results[1];
        }
        const std::string& s = node.children[0]->string_value;

        bool global_scope = node.type == Node::GLOBAL_ASSIGN || (
            !inside_function() && _scopes.back().symbol_table.size() <= 3 &&
            !_scopes.back().metadata.has(GLOBAL_DESTRUCTOR));
        if (!global_scope) {
          node.static_info.scope_number =
              _scopes.back().scope_numbering.back();
        }

        // Within global blocks, use the top-level symbol table frame.
        add_symbol_checking_collision(node, s, t, global_scope);
        auto& sym = (global_scope ? _scopes[0] : _scopes.back()).symbol_table;

        // Store global variables in the global map for future use.
        if (global_scope && node.type != Node::GLOBAL_ASSIGN) {
          bool exported = _scopes.back().metadata.has(EXPORT_GLOBAL);
          if (!t.is_error()) {
            _globals_output.emplace(s, t.external().make_exported(exported));
          }
          if (exported) {
            sym[s].warn_writes = sym[s].warn_reads = false;
          }
        }

        // Merge warnings for immediate assign hack.
        if (use_function_immediate_assign_hack(node)) {
          sym[s].warn_reads &= _immediate_left_assign_warn_reads;
        }
        sym[s].closed = sym[s].warn_closed =
            node.int_value & Node::MODIFIER_CLOSED;
        if (global_scope && sym[s].closed) {
          sym[s].warn_closed = false;
          error(node, "`closed` modifier has no effect on global " +
                      node.string_value, false);
        }

        // Only export functions get added to the function table. They also
        // are automatically assumed to be referenced.
        if (node.type == Node::GLOBAL_ASSIGN && !t.is_error() &&
            node.int_value & Node::MODIFIER_EXPORT) {
          _functions_output.emplace(s, t.external().make_exported(true));
          sym[s].warn_reads = false;
        }
        return results[1];
      });
      break;

    case Node::FUNCTION:
      _scopes.back().metadata.push();
      _scopes.back().metadata.add(TYPE_EXPR_CONTEXT, {});
      result(node, RESULT {
        return results[0].function() ? results[0] : Type(true);
      });

      call_after_result(*node.children[0], [&](const Type& result)
      {
        // Erase type context.
        _scopes.back().metadata.pop();
        // Make sure it's const so functions can't set themselves to different
        // values inside the body.
        Type t = result.make_const(true);
        if (!t.function()) {
          // Grammar no longer allows this, but leave it in for future-proofing.
          error(node, "function defined with non-function type " + str(t));
          t = Type(true);
        }

        // Functions need two symbol table frames: one for recursive hack, one
        // for the arguments, and one for the body.
        _scopes.emplace_back(node, _immediate_left_assign.length() ?
                                   _immediate_left_assign : "<anon>");
        push_symbol_tables();
        node.static_info.scope_number = _scopes.back().scope_numbering.back();
        if (_immediate_left_assign.length()) {
          // Immediate left assigns are implicitly closed.
          add_symbol(node, _immediate_left_assign, t, false);
          _scopes.back().symbol_table[_immediate_left_assign].closed = true;
          _immediate_left_assign = "";
        }

        // Do the arguments.
        push_symbol_tables();
        node.static_info.scope_number = _scopes.back().scope_numbering.back();
        if (!t.is_error()) {
          std::unordered_set<std::string> arg_names;
          std::size_t elem = 0;
          for (const auto& ptr : node.children[0]->children) {
            if (!elem) {
              ++elem;
              continue;
            }
            if (ptr->type != Node::NAMED_EXPRESSION) {
              // Don't add to the symbol table.
              continue;
            }
            const std::string& name = ptr->string_value;
            if (arg_names.find(name) != arg_names.end()) {
              error(*ptr, "duplicate argument name `" + name + "`");
            }
            // Arguments are implicitly const!
            Type u = (elem ? t.external().function_arg(elem - 1) :
                             t.external().function_return()).make_const(true);
            add_symbol(*ptr, name, u, false);
            // TODO: arguments are implicitly closed so far, since it's a bit
            // tricky to allow closed on arglists in the parser. Fix that maybe.
            _scopes.back().symbol_table[name].closed = true;
            arg_names.insert(name);
            ++elem;
          }
        }
        enter_function(t.external().function_return());
        push_symbol_tables();
      });

      call_after(node, [&](const result_list& results)
      {
        if (!current_return_type().is_void() &&
            (results[1].is_error() || !results[1].not_void())) {
          error(node, "not all code paths return a value");
        }
        pop_symbol_tables();
        pop_symbol_tables();
        // We don't pop the last symbol table (containing only the recursive
        // hack): instead we just want to merge its unreferenced warning
        // information with the symbol about to be added in the enclosing scope.
        std::vector<std::pair<std::string, symbol_t>> symbols;
        _scopes.back().symbol_table.get_symbols(symbols, 0, 2);
        for (const auto& pair : symbols) {
          _immediate_left_assign_warn_reads = pair.second.warn_reads;
        }
        _scopes.pop_back();
      });
      break;

    case Node::LOOP_AFTER_BLOCK:
    case Node::BLOCK:
      push_symbol_tables();
      call_after(node, [&]{pop_symbol_tables();});
      result(node, RESULT {
        // Check for dead code.
        bool dead = false;
        for (std::size_t i = 0; i < results.size(); ++i) {
          if (dead) {
            error(*node.children[i], "dead code", false);
            break;
          }
          dead |= !results[i].is_error();
        }
        // The code for RETURN_STMT checks return values against the function's
        // return type. We don't really care what types might be here, just any
        // non-void as a marker for ensuring all code paths return a value.
        for (const Type& t : results) {
          if (!t.is_error()) {
            return t;
          }
        }
        return Type(true);
      });
      break;

    case Node::IF_STMT:
      push_symbol_tables();
      call_after(node, [&]{pop_symbol_tables();});
      result(node, RESULT {
        if (!results[0].is(yang::Type::int_t())) {
          error(*node.children[0],
                str(node) + " branching on " + str(results[0]));
        }
        auto& body = results.size() > 2 ? node.children[2] : node.children[1];
        if (body->type == Node::EXPR_STMT &&
            body->children[0]->type == Node::EMPTY_EXPR) {
          error(*body, "empty statement in " + str(node), false);
        }
        // An IF_STMT definitely returns a value only if both branches
        // definitely return a value.
        Type left = results[1];
        Type right = results.size() > 2 ? results[2] : Type(true);
        return !left.is_error() && !right.is_error() ? left : Type(true);
      });
      break;

    case Node::FOR_STMT:
    case Node::WHILE_STMT:
    case Node::DO_WHILE_STMT:
      push_symbol_tables();
      call_after(node, [&]{pop_symbol_tables();});
      // Insert a marker into the symbol table that break and continue
      // statements can check for.
      _scopes.back().metadata.add(LOOP_BODY, {});
      // Separate scope for the loop after-statement.
      if (node.type == Node::FOR_STMT) {
        call_after(*node.children[1], [&]{_scopes.back().metadata.push();});
        call_after(*node.children[2], [&]{_scopes.back().metadata.pop();});
      }
      result(node, RESULT {
        std::size_t cond = node.type != Node::WHILE_STMT;
        if (!results[cond].is(yang::Type::int_t())) {
          error(*node.children[cond],
                str(node) + " branching on " + str(results[cond]));
        }
        return Type(true);
      });
      break;

    case Node::TERNARY:
      call_after_result(*node.children[0], [&](const Type& result)
      {
        if (!result.is_vector()) {
          push_symbol_tables();
          call_after(*node.children[1], [&]{
            pop_symbol_tables();
            push_symbol_tables();
          });
          call_after(*node.children[2], [&]{pop_symbol_tables();});
        }
      });
      result(node, RESULT {
        // The ternary operator vectorises, as in:
        // (a, b) ? (c, d) : (e, f) is equivalent to (a ? c : e, b ? d : f).
        //
        // It could also vectorise on the right (similar to binary operators),
        // as in: a ? (b, c) : d equivalent to a ? (b, c) : (d, d) or, also:
        // (a, b) ? (c, d) : e equivalent to (a, b) ? (c, d) : (e, e).
        // But, this is odd and confusing, so it's not allowed.
        bool err = false;
        if (!results[1].is(results[2])) {
          error(node, str(node) + " applied to " +
                      str(results[1]) + " and " + str(results[2]));
          err = true;
        }
        if (!results[0].is_int()) {
          error(node, str(node) + " branching on " + str(results[0]));
          err = true;
        }

        if (results[0].is_vector() && !err &&
            (!results[1].is_vector() || results[0].external().vector_size() !=
                                        results[1].external().vector_size())) {
          error(node, "length-" +
                      std::to_string(results[0].external().vector_size()) +
                      " vectorised branch applied to " +
                      str(results[1]) + " and " + str(results[2]));
        }
        return results[1].unify(results[2]);
      });
      break;

    case Node::CALL:
      if (inside_type_context()) {
        goto type_function;
      }
      result(node, RESULT {
        // The grammar doesn't distinguish function-type construction from
        // call-expressions in all contexts, so we need to check here.
        for (std::size_t i = 1; i < results.size(); ++i) {
          if (node.children[i]->type == Node::NAMED_EXPRESSION) {
            error(*node.children[i],
                  str(node) + ": named argument in function call");
          }
        }
        if (!results[0].function()) {
          error(node, str(node) + " applied to " + str(results[0]));
          return Type(true);
        }
        if (!results[0].element_size(results.size())) {
          error(node, str(results[0]) + " called with " +
                      std::to_string(results.size() - 1) + " argument(s)");
        }
        else {
          for (std::size_t i = 1; i < results.size(); ++i) {
            if (!results[0].element_is(i, results[i])) {
              error(*node.children[i],
                    str(results[0]) + " called with " + str(results[i]) +
                    " in position " + std::to_string(i - 1));
            }
          }
        }
        return results[0].is_error() ?
            Type(true) : results[0].external().function_return();
      });
      break;

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
      // Right-hand-sides of non-vectorised logical operators need an extra
      // scope, as they won't always be run.
      call_after_result(*node.children[0], [&](const Type& result)
      {
        if (!result.is_vector()) {
          push_symbol_tables();
          call_after(*node.children[1], [&]{pop_symbol_tables();});
        }
      });
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
      result(node, RESULT {
        // Takes two integers and produces an integer, with vectorisation.
        if (!results[0].is_binary_match(results[1])) {
          error(node, str(node) + " applied to " +
                      str(results[0]) + " and " + str(results[1]));
          return Type(true);
        }
        else if (!results[0].is_int() || !results[1].is_int()) {
          error(node, str(node) + " applied to " +
                      str(results[0]) + " and " + str(results[1]));
        }
        return binary_type(results[0], results[1], false);
      });
      break;

    default: {}
  }

#undef RESULT
  // Type/expression context error result overrides any other result.
  if (context_err) {
    result(node, []{return Type(true);});
  }
}

Type StaticChecker::after(const Node& node, const result_list& results)
{
  error(node, "unimplemented construct");
  return Type(true);
}

void StaticChecker::enter_function(const Type& return_type)
{
  _scopes.back().metadata.add(RETURN_TYPE, return_type);
}

const Type& StaticChecker::current_return_type() const
{
  return _scopes.back().metadata[RETURN_TYPE];
}

bool StaticChecker::inside_function() const
{
  return _scopes.back().metadata.has(RETURN_TYPE);
}

bool StaticChecker::inside_type_context() const
{
  return _scopes.back().metadata.has(TYPE_EXPR_CONTEXT);
}

void StaticChecker::push_symbol_tables()
{
  _scopes.back().symbol_table.push();
  _scopes.back().scope_numbering.push_back(
      _scopes.back().scope_numbering_next++);
}

void StaticChecker::pop_symbol_tables()
{
  _scopes.back().scope_numbering.pop_back();
  typedef std::pair<std::string, symbol_t> pair;
  std::vector<pair> symbols;
  std::size_t size = _scopes.back().symbol_table.size();
  _scopes.back().symbol_table.get_symbols(symbols, size - 1, size);

  for (const auto& p : symbols) {
    if (p.second.warn_writes && p.second.warn_reads) {
      error(*p.second.declaration,
            "symbol `" + p.first + "` is never referenced", false);
    }
    else if (p.second.warn_closed) {
      error(*p.second.declaration,
            "symbol `" + p.first + "` is never closed over", false);
    }
    else if (p.second.warn_writes) {
      error(*p.second.declaration,
            "symbol `" + p.first + "` is never written to", false);
    }
    else if (p.second.warn_reads) {
      error(*p.second.declaration,
            "symbol `" + p.first + "` is never read", false);
    }
  }
  _scopes.back().symbol_table.pop();
}

void StaticChecker::add_symbol(
    const Node& node, const std::string& name, const Type& type,
    bool global, bool unreferenced_warning)
{
  symbol_t sym;
  sym.type = type;
  sym.scope_number = global ? 0 : _scopes.back().scope_numbering.back();
  sym.declaration = &node;
  // We don't care if there are no writes to a const symbol, so we just set
  // warn_writes to false.
  unreferenced_warning = unreferenced_warning && !type.is_error();
  sym.warn_writes = unreferenced_warning && !type.external().is_const();
  sym.warn_reads = unreferenced_warning;
  (global ? _scopes[0] : _scopes.back()).symbol_table.add(name, sym);
}

void StaticChecker::add_symbol_checking_collision(
    const Node& node, const std::string& name, const Type& type,
    bool global, bool unreferenced_warning)
{
  auto& table = global ? _scopes[0].symbol_table : _scopes.back().symbol_table;
  if (table.has(name, table.size() - 1)) {
    // Skipping on error is debatable as to whether it really skips
    // unnecessary messages, or rather hides real name collisions.
    if (!table.get(name, table.size() - 1).type.is_error()) {
      error(node, (global ? "global " : "symbol ") +
                  ("`" + name + "` redefined"));
    }
    table.remove(name, table.size() - 1);
  }
  add_symbol(node, name, type, global, unreferenced_warning);
}

void StaticChecker::error(
    const Node& node, const std::string& message, bool error)
{
  std::string m = message;
  std::string function = "";
  for (const auto& scope : _scopes) {
    function += (function.length() ? "." : "") + scope.name;
  }
  if (function.length()) {
    m = "in function `" + function + "`: " + m;
  }
  auto error_info = _data.format_error(
      node.left_index, node.right_index,
      node.left_tree_index, node.right_tree_index, m, error);
  (error ? _data.errors : _data.warnings).push_back(error_info);
}

std::string StaticChecker::str(const Node& node) const
{
  return "`" + node_op_string(node.type) + "`";
}

std::string StaticChecker::str(const Type& type) const
{
  return type.string(_context);
}

StaticChecker::symbol_t::symbol_t()
  : closed(false)
  , scope_number(0)
  , declaration(nullptr)
  , warn_writes(false)
  , warn_reads(false)
  , warn_closed(false)
{
}

StaticChecker::lex_scope_t::lex_scope_t(
    const Node& function, const std::string& name)
  : function(function)
  , name(name)
  , metadata(Type())
  , symbol_table(symbol_t())
  , scope_numbering{0}
  , scope_numbering_next(1)
{
}

// End namespace yang::internal.
}
}
