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

void StaticChecker::preorder(const Node& node)
{
  // To reject type-expressions in non-type contexts and expressions in type-
  // -contexts without duplicating code everywhere, it's done here. But it does
  // mean we have to do a bit of awkward logic to make sure the correct tables
  // are popped on error.
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
  }

  switch (node.type) {
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
      if (node.children[0]->type == Node::IDENTIFIER) {
        add_symbol(node, node.children[0]->string_value, {}, false, false);
      }
      break;
    case Node::ASSIGN:
      // Change which warning references affect.
      _scopes.back().metadata.push();
      _scopes.back().metadata.add(ASSIGN_LHS_CONTEXT, {});
      break;
    case Node::FUNCTION:
      _scopes.back().metadata.push();
      _scopes.back().metadata.add(TYPE_EXPR_CONTEXT, {});
      break;
    case Node::BLOCK:
    case Node::IF_STMT:
      push_symbol_tables();
      break;
    case Node::DO_WHILE_STMT:
    case Node::FOR_STMT:
      push_symbol_tables();
      _scopes.back().metadata.push();
      // Insert a marker into the symbol table that break and continue
      // statements can check for.
      _scopes.back().metadata.add(LOOP_BODY, {});
      break;

    default: {}
  }
}

void StaticChecker::infix(const Node& node, const result_list& results)
{
  switch (node.type) {
    case Node::FUNCTION:
    {
      // Erase type context.
      _scopes.back().metadata.pop();
      // Make sure it's const so functions can't set themselves to different
      // values inside the body.
      Type t = results[0].make_const(true);
      if (!t.function()) {
        // Grammar no longer allows this, but leave it in for future-proofing.
        error(node, "function defined with non-function type " +
                    t.string(_context));
        t = Type(true);
      }

      // Functions need two symbol table frames: one for recursive hack, one for
      // the arguments, and one for the body.
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
          // tricky to allow closed on arglists in the parser. Fix that.
          _scopes.back().symbol_table[name].closed = true;
          arg_names.insert(name);
          ++elem;
        }
      }
      enter_function(t.external().function_return());
      push_symbol_tables();
      break;
    }

    case Node::GLOBAL_ASSIGN:
    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
      // Remove temporary reference to identifier on the left.
      pop_symbol_tables();
      break;
    case Node::ASSIGN:
      _scopes.back().metadata.pop();
      break;

    // Right-hand-sides of non-vectorised ternary and logical operators need an
    // extra scope, as they won't always be run.
    case Node::TERNARY:
      if (!results[0].is_vector() && results.size() == 2) {
        pop_symbol_tables();
      }
    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
      if (!results[0].is_vector()) {
        push_symbol_tables();
      }
      break;

    default: {}
  }
}

Type StaticChecker::visit(const Node& node, const result_list& results)
{
  auto element_type = [](const Type& t) -> Type
  {
    return t.is_error() ? Type(true) :
        t.is_int() ? yang::Type::int_t() :
        t.is_float() ? yang::Type::float_t() : Type(true);
  };
  auto numeric_type = [](bool is_float, std::size_t num) -> Type
  {
    return is_float ?
        (num == 1 ? yang::Type::float_t() : yang::Type::fvec_t(num)) :
        (num == 1 ? yang::Type::int_t() : yang::Type::ivec_t(num));
  };
  auto binary_type = [&](const Type& a, const Type& b, bool to_int) -> Type
  {
    if (a.is_error() || b.is_error()) {
      return Type(true);
    }
    std::size_t max = std::max(a.external().vector_size(),
                               b.external().vector_size());
    return numeric_type(results[0].is_float() && !to_int, max);
  };
  std::string s = "`" + node_op_string(node.type) + "`";
  std::vector<std::string> rs;
  for (const Type& t : results) {
    rs.push_back(t.string(_context));
  }

  // Pop the correct tables before returning an error. Make sure to do this
  // before checking for context error; otherwise we might still think we're
  // in a type-context when we aren't.
  if (node.type == Node::GLOBAL) {
    pop_symbol_tables();
    _scopes.pop_back();
  }
  else if (node.type == Node::DO_WHILE_STMT || node.type == Node::FOR_STMT) {
    _scopes.back().metadata.pop();
    pop_symbol_tables();
  }
  else if (node.type == Node::FUNCTION) {
    // FUNCTION needs to do some logic before popping the tables.
    if (!current_return_type().is_void() &&
        (results[1].is_error() || !results[1].not_void())) {
      error(node, "not all code paths return a value");
    }
    pop_symbol_tables();
    pop_symbol_tables();
    // We don't pop the last symbol table (containing only the recursive hack):
    // instead we just want to merge its unreferenced warning information with
    // the symbol about to be added in the enclosing scope.
    std::vector<std::pair<std::string, symbol_t>> symbols;
    _scopes.back().symbol_table.get_symbols(symbols, 0, 2);
    for (const auto& pair : symbols) {
      _immediate_left_assign_warn_reads = pair.second.warn_reads;
    }
    _scopes.pop_back();
  }
  else if (node.type == Node::BLOCK || node.type == Node::IF_STMT) {
    pop_symbol_tables();
  }

  // See preorder for error.
  bool context_err = !valid_all_contexts(node) &&
      inside_type_context() != is_type_expression(node);
  if (context_err) {
    // And make sure to pop the override context.
    _scopes.back().metadata.pop();
    return Type(true);
  }

  // To make the error messages useful, the general idea here is to fall back to
  // the ERROR type only when the operands (up to errors) do not uniquely
  // determine some result type.
  // For example, the erroneous (1, 1) + (1, 1, 1) results in an ERROR type,
  // since there's no way to decide if an int2 or int3 was intended. However,
  // the erroneous 1 == 1. gives type INT, as the result would be INT whether or
  // not the operand type was intended to be int or float.
  switch (node.type) {
    case Node::TYPE_VOID:
      return {};
    case Node::TYPE_INT:
      return numeric_type(false, node.int_value);
    case Node::TYPE_FLOAT:
      return numeric_type(true, node.int_value);
    case Node::TYPE_FUNCTION:
    {
      type_function:
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
    }

    case Node::PROGRAM:
      // Make sure to warn on unused top-level elements. This doesn't actually
      // pop anything, since it's the last frame.
      pop_symbol_tables();
    case Node::GLOBAL:
      return {};
    case Node::GLOBAL_ASSIGN:
    {
      if (node.children[0]->type != Node::IDENTIFIER) {
        if (!results[0].is_error()) {
          error(node, "assignments must be directly to an identifier");
        }
        return {};
      }
      const std::string& s = node.children[0]->string_value;
      Type t = results[1].make_const(true);
      if (!results[1].function()) {
        error(node, "global assignment of type " + rs[1]);
        add_symbol_checking_collision(node, s, results[1], true, false);
      }
      else {
        add_symbol_checking_collision(node, s, t, true, !t.is_error());
        _scopes[0].symbol_table[s].warn_reads &=
            _immediate_left_assign_warn_reads;
      }
      // Only export functions get added to the function table. They also are
      // automatically assumed to be referenced.
      if (!t.is_error() && node.int_value & Node::MODIFIER_EXPORT) {
        _functions_output.emplace(s, t.external().make_exported(true));
        _scopes[0].symbol_table[s].warn_reads = false;
      }
      return {};
    }
    case Node::FUNCTION:
    {
      // We've already reported an error in infix() if the first type is not
      // a function type.
      return results[0].function() ? results[0] : Type(true);
    }
    case Node::NAMED_EXPRESSION:
      return results[0];

    case Node::BLOCK:
    {
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
    }

    case Node::EMPTY_STMT:
    case Node::EXPR_STMT:
      return Type(true);
    case Node::RETURN_VOID_STMT:
    case Node::RETURN_STMT:
    {
      Type t = node.type == Node::RETURN_STMT ? results[0] : Type();
      // If we're not in a function, we must be in a global block.
      if (!inside_function()) {
        error(node, "return statement inside `global`");
        return t;
      }
      const Type& current_return = current_return_type();
      if (!t.is(current_return)) {
        const auto& n = node.type == Node::RETURN_STMT ?
            *node.children[0] : node;
        error(n, "returning " + t.string(_context) + " from " +
                 current_return.string(_context) + " function");
      }
      return t;
    }
    case Node::IF_STMT:
    {
      if (!results[0].is(yang::Type::int_t())) {
        error(*node.children[0], "branching on " + rs[0]);
      }
      if (results.size() > 2) {
        if (node.children[2]->type == Node::EMPTY_STMT) {
          error(*node.children[2], "empty statement in `else`", false);
        }
      }
      else if (node.children[1]->type == Node::EMPTY_STMT) {
        error(*node.children[1], "empty statement in `if`", false);
      }
      // An IF_STMT definitely returns a value only if both branches definitely
      // return a value.
      Type left = results[1];
      Type right = results.size() > 2 ? results[2] : Type(true);
      return !left.is_error() && !right.is_error() ? left : Type(true);
    }
    case Node::DO_WHILE_STMT:
    case Node::FOR_STMT:
      if (!results[1].is(yang::Type::int_t())) {
        error(*node.children[1], "branching on " + rs[1]);
      }
      return Type(true);
    case Node::BREAK_STMT:
      if (!_scopes.back().metadata.has(LOOP_BODY)) {
        error(node, "`break` outside of loop body");
      }
      return Type(true);
    case Node::CONTINUE_STMT:
      if (!_scopes.back().metadata.has(LOOP_BODY)) {
        error(node, "`continue` outside of loop body");
      }
      return Type(true);

    case Node::MEMBER_SELECTION:
    {
      if (!results[0].user_type()) {
        error(node, "member function access on " + rs[0]);
        return Type(true);
      }
      if (results[0].is_error()) {
        return Type(true);
      }

      const auto& m =
          _context.member_lookup(results[0].external(), node.string_value);
      if (m.type.is_void()) {
        error(
            node, "undeclared member function `" +
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
    }

    case Node::IDENTIFIER:
    {
      // Look up user types in a type-context.
      if (inside_type_context()) {
        const auto& t = _context.type_lookup(node.string_value);
        if (!t.type.is_void()) {
          return t.type;
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
          const auto& t = _context.type_lookup(node.string_value);
          if (t.type.is_managed_user_type()) {
            // Fix the constructor return type.
            std::vector<yang::Type> args;
            for (std::size_t i = 0;
                 i < t.constructor.type.function_num_args(); ++i) {
              args.push_back(t.constructor.type.function_arg(i));
            }
            return yang::Type::function_t(t.type, args);
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
      // If this is a reference to variable in an enclosing function, make sure
      // it's added to that function's closed environment.
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
      (_scopes.back().metadata.has(ASSIGN_LHS_CONTEXT) ?
           symbol.warn_writes : symbol.warn_reads) = false;
      return symbol.type;
    }
    case Node::INT_LITERAL:
      return yang::Type::int_t();
    case Node::FLOAT_LITERAL:
      return yang::Type::float_t();

    case Node::TERNARY:
    {
      if (!results[0].is_vector()) {
        pop_symbol_tables();
      }
      // The ternary operator vectorises, as in:
      // (a, b) ? (c, d) : (e, f) is equivalent to (a ? c : e, b ? d : f).
      //
      // It could also vectorise on the right (similar to binary operators), as
      // in: a ? (b, c) : d equivalent to a ? (b, c) : (d, d) or, also:
      // (a, b) ? (c, d) : e equivalent to (a, b) ? (c, d) : (e, e).
      // But, this is odd and confusing, so it's not allowed.
      bool err = false;
      if (!results[1].is(results[2])) {
        error(node, s + " applied to " + rs[1] + " and " + rs[2]);
        err = true;
      }
      if (!results[0].is_int()) {
        error(node, s + " branching on " + rs[0]);
        err = true;
      }

      if (results[0].is_vector() && !err &&
          (!results[1].is_vector() || results[0].external().vector_size() !=
                                      results[1].external().vector_size())) {
        error(node, "length-" +
                    std::to_string(results[0].external().vector_size()) +
                    " vectorised branch applied to " + rs[1] + " and " + rs[2]);
      }
      return results[1].unify(results[2]);
    }
    case Node::CALL:
      // The grammar doesn't distinguish function-type construction from
      // call-expressions in all contexts, so we need to check here.
      if (inside_type_context()) {
        goto type_function;
      }

      for (std::size_t i = 1; i < results.size(); ++i) {
        if (node.children[i]->type == Node::NAMED_EXPRESSION) {
          error(*node.children[i], s + ": named argument in function call");
        }
      }
      if (!results[0].function()) {
        error(node, s + " applied to " + rs[0]);
        return Type(true);
      }
      if (!results[0].element_size(results.size())) {
        error(node, rs[0] + " called with " +
                    std::to_string(results.size() - 1) + " argument(s)");
      }
      else {
        for (std::size_t i = 1; i < results.size(); ++i) {
          if (!results[0].element_is(i, results[i])) {
            error(*node.children[i], rs[0] + " called with " + rs[i] +
                                     " in position " + std::to_string(i - 1));
          }
        }
      }
      return results[0].is_error() ?
          Type(true) : results[0].external().function_return();

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
      if (!results[0].is_vector()) {
        pop_symbol_tables();
      }
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
    {
      // Takes two integers and produces an integer, with vectorisation.
      if (!results[0].is_binary_match(results[1])) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type(true);
      }
      else if (!results[0].is_int() || !results[1].is_int()) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
      }
      return binary_type(results[0], results[1], true);
    }

    case Node::POW:
    case Node::MOD:
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
    {
      // Takes two integers or floats and produces a value of the same type,
      // with vectorisation.
      if (!results[0].is_binary_match(results[1]) ||
          (!(results[0].is_int() && results[1].is_int()) &&
           !(results[0].is_float() && results[1].is_float()))) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type(true);
      }
      return binary_type(results[0], results[1], false);
    }

    case Node::EQ:
    case Node::NE:
    case Node::GE:
    case Node::LE:
    case Node::GT:
    case Node::LT:
    {
      // Takes two integers or floats and produces an integer, with
      // vectorisation.
      if (!results[0].is_binary_match(results[1])) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type(true);
      }
      else if (!(results[0].is_int() && results[1].is_int()) &&
               !(results[0].is_float() && results[1].is_float())) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
      }
      return binary_type(results[0], results[1], true);
    }

    case Node::FOLD_LOGICAL_OR:
    case Node::FOLD_LOGICAL_AND:
    case Node::FOLD_BITWISE_OR:
    case Node::FOLD_BITWISE_AND:
    case Node::FOLD_BITWISE_XOR:
    case Node::FOLD_BITWISE_LSHIFT:
    case Node::FOLD_BITWISE_RSHIFT:
      if (!results[0].is_vector() || !results[0].is_int()) {
        error(node, s + " applied to " + rs[0]);
      }
      return yang::Type::int_t();

    case Node::FOLD_POW:
    case Node::FOLD_MOD:
    case Node::FOLD_ADD:
    case Node::FOLD_SUB:
    case Node::FOLD_MUL:
    case Node::FOLD_DIV:
      if (!results[0].is_vector() ||
          !(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
        return Type(true);
      }
      return element_type(results[0]);

    case Node::FOLD_EQ:
    case Node::FOLD_NE:
    case Node::FOLD_GE:
    case Node::FOLD_LE:
    case Node::FOLD_GT:
    case Node::FOLD_LT:
      if (!results[0].is_vector() ||
          !(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
      }
      return yang::Type::int_t();

    case Node::LOGICAL_NEGATION:
    case Node::BITWISE_NEGATION:
      if (!results[0].is_int()) {
        error(node, s + " applied to " + rs[0]);
      }
      return numeric_type(false, results[0].external().vector_size());

    case Node::ARITHMETIC_NEGATION:
      if (!(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
        return Type(true);
      }
      return results[0];
    case Node::INCREMENT:
    case Node::DECREMENT:
    {
      if (!(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
        return Type(true);
      }
      Type t = results[0];
      if (node.children[0]->type == Node::IDENTIFIER) {
        if (results[0].external().is_const()) {
          error(node, s + " applied to " + rs[0]);
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
    }

    case Node::ASSIGN:
    {
      if (node.children[0]->type != Node::IDENTIFIER) {
        if (!results[0].is_error()) {
          error(node, "assignments must be directly to an identifier");
        }
        return results[1];
      }
      const std::string& s = node.children[0]->string_value;
      for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it) {
        if (!it->symbol_table.has(s)) {
          continue;
        }
        Type t = it->symbol_table[s].type;
        if (!t.is(results[1])) {
          error(node, rs[1] + " assigned to `" + s + "` of type " +
                      t.string(_context));
          return Type(true);
        }
        else if (t.external().is_const()) {
          error(node, "assignment to `" + s + "` of type " +
                      t.string(_context));
        }
        return results[1];
      }

      if (!_context.function_lookup(s).type.is_void()) {
        error(node, "cannot assign to context function `" + s + "`");
      }
      else if (!_context.type_lookup(s).type.is_void()) {
        error(node, "cannot assign to context type `" + s + "`");
      }
      else {
        error(node, "undeclared identifier `" + s + "`");
      }
      add_symbol(node, s, results[1], false, false);
      return results[1];
    }

    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
    {
      if (!results[1].not_void()) {
        error(node, "assignment of type " + rs[1]);
      }
      if (node.children[0]->type != Node::IDENTIFIER) {
        error(node, "assignments must be directly to an identifier");
        return results[1];
      }
      const std::string& s = node.children[0]->string_value;

      auto add_global = [&]
      {
        bool exported = _scopes.back().metadata.has(EXPORT_GLOBAL);
        const Type& t = _scopes[0].symbol_table.get(s, 0).type;
        if (!t.is_error()) {
          _globals_output.emplace(s, t.external().make_exported(exported));
        }
        if (exported) {
          _scopes[0].symbol_table.get(s, 0).warn_writes = false;
          _scopes[0].symbol_table.get(s, 0).warn_reads = false;
        }
      };
      bool global =
          !inside_function() && _scopes.back().symbol_table.size() <= 3 &&
          !_scopes.back().metadata.has(GLOBAL_DESTRUCTOR);
      if (!global) {
        node.static_info.scope_number = _scopes.back().scope_numbering.back();
      }

      Type t = results[1].make_const(node.type == Node::ASSIGN_CONST);
      // Merge warnings for immediate assign hack.
      bool warn_reads = true;
      if (use_function_immediate_assign_hack(node)) {
        warn_reads &= _immediate_left_assign_warn_reads;
      }

      // Within global blocks, use the top-level symbol table frame.
      if (global) {
        add_symbol_checking_collision(node, s, t, true);
        // Store global in the global map for future use.
        add_global();
      }
      else {
        add_symbol_checking_collision(node, s, t, false);
      }
      auto& sym = (global ? _scopes[0] : _scopes.back()).symbol_table[s];
      sym.warn_reads &= warn_reads;
      sym.closed = sym.warn_closed = node.int_value & Node::MODIFIER_CLOSED;
      if (global && sym.closed) {
        sym.warn_closed = false;
        error(node, "`closed` modifier has no effect on global " +
                    node.string_value, false);
      }
      return results[1];
    }

    case Node::INT_CAST:
      if (!results[0].is_float()) {
        error(node, s + " applied to " + rs[0]);
      }
      return numeric_type(false, results[0].external().vector_size());

    case Node::FLOAT_CAST:
      if (!results[0].is_int()) {
        error(node, s + " applied to " + rs[0]);
      }
      return numeric_type(true, results[0].external().vector_size());

    case Node::VECTOR_CONSTRUCT:
    {
      Type t = results[0];
      std::string ts;
      bool unify_error = false;
      std::unordered_set<Type> bad_types;
      for (std::size_t i = 0; i < results.size(); ++i) {
        if (node.children[i]->type == Node::NAMED_EXPRESSION) {
          error(*node.children[i],
                s + ": named argument in vector construction");
        }
        if (!results[i].primitive() && !bad_types.count(results[i])) {
          // Store the bad types we saw already so as not to repeat the error.
          error(*node.children[i], s + ": element with non-primitive type " +
                                   rs[i] + " in vector construction");
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
        ts += rs[i];
      }
      if (unify_error) {
        error(node, s + " applied to different types " + ts);
        return Type(true);
      }
      return numeric_type(t.is_float(), results.size());
    }
    case Node::VECTOR_INDEX:
      if (!results[0].is_vector() || !results[1].is(yang::Type::int_t())) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return results[0].is_vector() ? element_type(results[0]) : Type(true);
      }
      return element_type(results[0]);

    default:
      error(node, "unimplemented construct");
      return Type(true);
  }
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

bool StaticChecker::is_type_expression(const Node& node) const
{
  return
      node.type == Node::TYPE_VOID || node.type == Node::TYPE_INT ||
      node.type == Node::TYPE_FLOAT || node.type == Node::TYPE_FUNCTION;
}

bool StaticChecker::valid_all_contexts(const Node& node) const
{
  // IDENTIFIER, NAMED_EXPRESSION, and CALL are currently the only things that
  // make sense in both contexts.
  return
      node.type == Node::IDENTIFIER || node.type == Node::NAMED_EXPRESSION ||
      node.type == Node::CALL;
}

bool StaticChecker::inside_type_context() const
{
  return _scopes.back().metadata.has(TYPE_EXPR_CONTEXT);
}

bool StaticChecker::use_function_immediate_assign_hack(const Node& node) const
{
  // Node should be type GLOBAL_ASSIGN, ASSIGN_VAR or ASSIGN_CONST.
  return node.children[0]->type == Node::IDENTIFIER &&
      node.children[1]->type == Node::FUNCTION;
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
