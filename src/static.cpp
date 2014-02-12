//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "static.h"

#include <unordered_set>
#include <vector>
#include <yang/context.h>
#include "log.h"

namespace std {
  template<>
  struct hash<yang::internal::StaticChecker::metadata> {
    std::size_t operator()(yang::internal::StaticChecker::metadata v) const
    {
      return v;
    }
  };
}

namespace yang {
namespace internal {

StaticChecker::StaticChecker(
    const yang::Context& context, std::string* error_output,
    symbol_frame& functions_output, symbol_frame& globals_output)
  : _errors(false)
  , _metadata(Type::VOID)
  , _symbol_table(Type::VOID)
  , _context(context)
  , _error_output(error_output)
  , _functions_output(functions_output)
  , _globals_output(globals_output)
{
}

StaticChecker::~StaticChecker()
{
  // Keep hash<metadata> in source file.
}

bool StaticChecker::errors() const
{
  return _errors;
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
    if (!_metadata.has(ERR_EXPR_CONTEXT)) {
      if (inside_type_context()) {
        error(node, "expected type in this context");
      }
      else {
        error(node, "unexpected type in this context");
      }
    }
    // Avoid duplicated errors by adding an override context.
    _metadata.push();
    _metadata.add(ERR_EXPR_CONTEXT, Type::VOID);
  }

  switch (node.type) {
    case Node::GLOBAL:
      _current_function = ".global";
      _symbol_table.push();
      _metadata.push();
      if (node.int_value) {
        _metadata.add(EXPORT_GLOBAL, Type::VOID);
      }
      break;
    case Node::GLOBAL_ASSIGN:
      // Set current top-level function name.
      if (node.children[0]->type == Node::IDENTIFIER) {
        _current_function = node.children[0]->string_value;
      }
      // Fall-through to recursion handler.
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
      _symbol_table.push();
      if (node.children[0]->type == Node::IDENTIFIER) {
        _symbol_table.add(node.children[0]->string_value, Type::VOID);
      }
      break;
    case Node::FUNCTION:
    case Node::SCOPE_RESOLUTION:
      _metadata.push();
      _metadata.add(TYPE_EXPR_CONTEXT, Type::VOID);
      break;
    case Node::BLOCK:
    case Node::IF_STMT:
      _symbol_table.push();
      break;
    case Node::DO_WHILE_STMT:
    case Node::FOR_STMT:
      _symbol_table.push();
      _metadata.push();
      // Insert a marker into the symbol table that break and continue
      // statements can check for.
      _metadata.add(LOOP_BODY, Type::VOID);
      break;
    case Node::CALL:
      _metadata.push();
      _metadata.add(CALLEE_CONTEXT, Type::VOID);
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
      _metadata.pop();
      // Only append a suffix if this isn't a top-level function. Make use of
      // the recursive name hack, if it's there.
      if (inside_function()) {
        _current_function += _immediate_left_assign.length() ?
            "." + _immediate_left_assign : ".anon";
      }
      Type t = results[0];
      // Make sure it's const so functions can't set themselves to different
      // values inside the body.
      t.set_const(true);
      if (!t.function()) {
        // Grammar no longer allows this, but leave it in for future-proofing.
        error(node, "function defined with non-function type " + t.string());
        t = Type::ERROR;
      }

      // Functions need three symbol table frames: one to store the function's
      // enclosing-function-reference overrides; one for arguments; and one for
      // the body. The immediate-name-assign hack goes in the previous frame.
      std::unordered_set<std::string> locals;
      _symbol_table.get_symbols(locals, 1, _symbol_table.size());
      // Do the recursive hack.
      if (_immediate_left_assign.length()) {
        add_symbol_checking_collision(
            node, _immediate_left_assign,
            inside_function() * (_symbol_table.size() - 1), t);
      }
      _symbol_table.push();
      // We currently don't implement closures at all, so we need to stick a
      // bunch of overrides into an intermediate stack frame to avoid the locals
      // from the enclosing scope being referenced, if any.
      // Starting at frame 1 finds all names except globals.
      // TODO: get rid of all this nonsense.
      for (const std::string& s : locals) {
        _symbol_table.add(s, Type::ENCLOSING_FUNCTION);
      }
      // We also need to make sure top-level function names get overridden in
      // inner scopes, until closures are implemented.
      if (_immediate_left_assign.length()) {
        if (!_symbol_table.has(_immediate_left_assign,
                               _symbol_table.size() - 1)) {
          _symbol_table.add(_immediate_left_assign,
                            _symbol_table[_immediate_left_assign]);
        }
        // Move this back to the place above when closures are in.
        _immediate_left_assign = "";
      }

      // Do the arguments.
      _symbol_table.push();
      if (!t.is_error()) {
        std::unordered_set<std::string> arg_names;
        std::size_t elem = 0;
        for (const auto& ptr : node.children[0]->children) {
          if (!elem) {
            ++elem;
            continue;
          }
          if (ptr->type != Node::NAMED_EXPRESSION) {
            // TODO: this shouldn't be an error if the argument isn't used.
            error(*ptr, "unnamed argument in function definition");
            continue;
          }
          const std::string& name = ptr->string_value;
          if (arg_names.find(name) != arg_names.end()) {
            error(*ptr, "duplicate argument name `" + name + "`");
          }
          _symbol_table.add(name, t.elements(elem));
          arg_names.insert(name);
          ++elem;
        }
      }
      // Stores the return type of the current function and the fact we're
      // inside a function.
      _metadata.push();
      enter_function(t.elements(0));
      _symbol_table.push();
      break;
    }

    case Node::GLOBAL_ASSIGN:
    case Node::ASSIGN_VAR:
    case Node::ASSIGN_CONST:
      // Remove temporary reference to identifier on the left.
      _symbol_table.pop();
      break;

    case Node::CALL:
      if (results.size() == 1) {
        _metadata.pop();
      }
      break;

    default: {}
  }
}

Type StaticChecker::visit(const Node& node, const result_list& results)
{
  std::string s = "`" + Node::op_string(node.type) + "`";
  std::vector<std::string> rs;
  for (const Type& t : results) {
    rs.push_back(t.string());
  }

  // Pop the correct tables before returning an error. Make sure to do this
  // before checking for context error; otherwise we might still think we're
  // in a type-context when we aren't.
  if (node.type == Node::GLOBAL ||
      node.type == Node::DO_WHILE_STMT || node.type == Node::FOR_STMT) {
    _metadata.pop();
    _symbol_table.pop();
  }
  else if (node.type == Node::FUNCTION) {
    // FUNCTION needs to do some logic before popping the tables.
    if (!current_return_type().is_void() && !results[1].not_void()) {
      error(node, "not all code paths return a value");
    }
    _symbol_table.pop();
    _symbol_table.pop();
    _symbol_table.pop();
    _metadata.pop();
  }
  else if (node.type == Node::BLOCK || node.type == Node::IF_STMT) {
    _symbol_table.pop();
  }
  // Make sure to pop callee context on nullary calls.
  else if (node.type == Node::SCOPE_RESOLUTION ||
           (node.type == Node::CALL && results.size() == 1)) {
    _metadata.pop();
  }

  bool context_err = !valid_all_contexts(node) &&
      inside_type_context() != is_type_expression(node);
  if (context_err) {
    // And make sure to pop the override context.
    _metadata.pop();
    return Type::ERROR;
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
      return Type::VOID;
    case Node::TYPE_INT:
      return Type(Type::INT, node.int_value);
    case Node::TYPE_FLOAT:
      return Type(Type::FLOAT, node.int_value);
    case Node::TYPE_FUNCTION:
    {
      type_function:
      bool err = results[0].is_error();
      Type t(Type::FUNCTION, results[0]);
      for (std::size_t i = 1; i < results.size(); ++i) {
        if (!results[i].not_void()) {
          error(node, "function type with `void` argument type");
        }
        if (results[i].is_error()) {
          err = true;
        }
        t.add_element(results[i]);
      }
      return err ? Type::ERROR : t;
    }

    case Node::PROGRAM:
      return Type::VOID;
    case Node::GLOBAL:
      _current_function = "";
      return Type::VOID;
    case Node::GLOBAL_ASSIGN:
    {
      if (node.children[0]->type != Node::IDENTIFIER) {
        error(node, "assignments must be directly to an identifier");
        return Type::VOID;
      }
      const std::string& s = node.children[0]->string_value;
      if (!results[1].function()) {
        error(node, "global assignment of type " + rs[1]);
        add_symbol_checking_collision(node, s, results[1]);
      }
      // Otherwise, the symbol already exists by the immediate-name-assign
      // recursion hack.
      const Type& t = _symbol_table.get(s, 0);
      // Only export functions get added to the function table.
      if (!t.is_error() && node.int_value) {
        _functions_output.emplace(s, t.external(true));
      }
      _current_function = "";
      return Type::VOID;
    }
    case Node::FUNCTION:
    {
      if (inside_function()) {
        _current_function =
            _current_function.substr(0, _current_function.find_last_of('.'));
      }
      // We've already reported an error in infix() if the first type is not
      // a function type.
      return results[0].function() ? results[0] : Type::ERROR;
    }
    case Node::NAMED_EXPRESSION:
      return results[0];

    case Node::BLOCK:
    {
      // The code for RETURN_STMT checks return values against the function's
      // return type. We don't really care what types might be here, just any
      // non-void as a marker for ensuring all code paths return a value.
      for (const Type& t : results) {
        if (t.not_void()) {
          return t;
        }
      }
      return Type::VOID;
    }

    case Node::EMPTY_STMT:
    case Node::EXPR_STMT:
      return Type::VOID;
    case Node::RETURN_STMT:
      // If we're not in a function, we must be in a global block.
      if (!inside_function()) {
        error(node, "return statement inside `global`");
      }
      else {
        const Type& current_return = current_return_type();
        if (!results[0].is(current_return)) {
          error(node, "returning " + rs[0] + " from " +
                      current_return.string() + " function");
        }
      }
      return results[0];
    case Node::IF_STMT:
    {
      if (!results[0].is(Type::INT)) {
        error(node, "branching on " + rs[0]);
      }
      // An IF_STMT definitely returns a value only if both branches definitely
      // return a value.
      Type left = results[1];
      Type right = results.size() > 2 ? results[2] : Type::VOID;
      return left.not_void() && right.not_void() ? left : Type::VOID;
    }
    case Node::DO_WHILE_STMT:
    case Node::FOR_STMT:
      if (!results[1].is(Type::INT)) {
        error(node, "branching on " + rs[1]);
      }
      return Type::VOID;
    case Node::BREAK_STMT:
      if (!_metadata.has(LOOP_BODY)) {
        error(node, "`break` outside of loop body");
      }
      return Type::VOID;
    case Node::CONTINUE_STMT:
      if (!_metadata.has(LOOP_BODY)) {
        error(node, "`continue` outside of loop body");
      }
      return Type::VOID;

    case Node::SCOPE_RESOLUTION:
    {
      if (!results[0].user_type()) {
        error(node, s + " applied to " + rs[0]);
        return Type::ERROR;
      }
      if (results[0].is_error()) {
        return Type::ERROR;
      }
      node.user_type_name = results[0].user_type_name();
      std::string s = node.user_type_name + "::" + node.string_value;
      auto context_it = _context.get_functions().find(s);
      if (context_it == _context.get_functions().end()) {
        error(node, "undeclared member function `" + s + "`");
        return Type::ERROR;
      }
      return context_it->second.type;
    }

    case Node::MEMBER_SELECTION:
    {
      // Without something like closures (to store the user object), it's not
      // possible to do much with a member function access other than call it
      // immediately.
      if (!results[0].user_type()) {
        error(node, "member function access on " + rs[0]);
        return Type::ERROR;
      }
      if (!_metadata.has(CALLEE_CONTEXT)) {
        error(node, "member function access outside of call context");
        return Type::ERROR;
      }
      if (results[0].is_error()) {
        return Type::ERROR;
      }

      node.user_type_name = results[0].user_type_name();
      std::string s = node.user_type_name + "::" + node.string_value;
      auto it = _context.get_functions().find(s);
      if (it == _context.get_functions().end()) {
        error(node, "undeclared member function `" + s + "`");
        return Type::ERROR;
      }
      // Omit the first argument (self). Unfortunately, the indirection here
      // makes errors when calling the returned function somewhat vague.
      const yang::Type& t = it->second.type;
      Type member = Type(Type::FUNCTION, t.get_function_return_type());
      for (std::size_t i = 1; i < t.get_function_num_args(); ++i) {
        member.add_element(t.get_function_arg_type(i));
      }
      return member;
    }

    case Node::IDENTIFIER:
    {
      // Look up user types in a type-context.
      if (inside_type_context()) {
        auto context_it = _context.get_types().find(node.string_value);
        if (context_it == _context.get_types().end()) {
          error(node, "undeclared type `" + node.string_value + "`");
          return Type::ERROR;
        }
        return Type(Type::USER_TYPE, node.string_value);
      }

      // Check Context if symbol isn't present in the Program table.
      bool has = _symbol_table.has(node.string_value);
      auto context_it = _context.get_functions().find(node.string_value);
      if (!has && context_it != _context.get_functions().end()) {
        return context_it->second.type;
      }

      // Regular program symbols.
      if (!has) {
        error(node, "undeclared identifier `" + node.string_value + "`");
        _symbol_table.add(node.string_value, Type::ERROR);
      }
      else if (_symbol_table[node.string_value] == Type::ENCLOSING_FUNCTION) {
        error(node, "reference to `" + node.string_value +
                    "` in enclosing function");
        return Type::ERROR;
      }
      return _symbol_table[node.string_value];
    }
    case Node::INT_LITERAL:
      return Type::INT;
    case Node::FLOAT_LITERAL:
      return Type::FLOAT;

    case Node::TERNARY:
    {
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
          (!results[1].is_vector() || results[0].count() != results[1].count())) {
        error(node, "length-" + std::to_string(results[0].count()) +
                    " vectorised branch applied to " +
                    rs[1] + " and " + rs[2]);
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
        return Type::ERROR;
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
      return results[0].elements(0);

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
      // Takes two integers and produces an integer, with vectorisation.
      if (!results[0].count_binary_match(results[1])) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type::ERROR;
      }
      else if (!results[0].is_int() || !results[1].is_int()) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
      }
      return Type(Type::INT, std::max(results[0].count(), results[1].count()));

    case Node::POW:
    case Node::MOD:
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
      // Takes two integers or floats and produces a value of the same type,
      // with vectorisation.
      if (!results[0].count_binary_match(results[1]) ||
          (!(results[0].is_int() && results[1].is_int()) &&
           !(results[0].is_float() && results[1].is_float()))) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type::ERROR;
      }
      return Type(results[0].base(),
                  std::max(results[0].count(), results[1].count()));

    case Node::EQ:
    case Node::NE:
    case Node::GE:
    case Node::LE:
    case Node::GT:
    case Node::LT:
      // Takes two integers or floats and produces an integer, with
      // vectorisation.
      if (!results[0].count_binary_match(results[1])) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return Type::ERROR;
      }
      else if (!(results[0].is_int() && results[1].is_int()) &&
               !(results[0].is_float() && results[1].is_float())) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
      }
      return Type(Type::INT, std::max(results[0].count(), results[1].count()));

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
      return Type::INT;

    case Node::FOLD_POW:
    case Node::FOLD_MOD:
    case Node::FOLD_ADD:
    case Node::FOLD_SUB:
    case Node::FOLD_MUL:
    case Node::FOLD_DIV:
      if (!results[0].is_vector() ||
          !(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
        return Type::ERROR;
      }
      return results[0].base();

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
      return Type::INT;

    case Node::LOGICAL_NEGATION:
    case Node::BITWISE_NEGATION:
      if (!results[0].is_int()) {
        error(node, s + " applied to " + rs[0]);
      }
      return Type(Type::INT, results[0].count());

    case Node::ARITHMETIC_NEGATION:
      if (!(results[0].is_int() || results[0].is_float())) {
        error(node, s + " applied to " + rs[0]);
        return Type::ERROR;
      }
      return results[0];

    case Node::ASSIGN:
    {
      if (node.children[0]->type != Node::IDENTIFIER) {
        error(node, "assignments must be directly to an identifier");
        return results[1];
      }
      const std::string& s = node.children[0]->string_value;
      if (!_symbol_table.has(s)) {
        if (_context.get_functions().count(s)) {
          error(node, "cannot assign to context function `" + s + "`");
        }
        else if (_context.get_types().count(s)) {
          error(node, "cannot assign to context type `" + s + "`");
        }
        else {
          error(node, "undeclared identifier `" + s + "`");
        }
        _symbol_table.add(s, results[1]);
        return results[1];
      }
      Type& t = _symbol_table[s];
      if (t == Type::ENCLOSING_FUNCTION) {
        error(node, "reference to `" + s + "` in enclosing function");
        return Type::ERROR;
      }
      if (t.is_const()) {
        error(node, "assignment to `" + s + "` of type " + t.string());
      }
      if (!t.is(results[1])) {
        error(node, rs[1] + " assigned to `" + s + "` of type " + t.string());
      }
      t = results[1];
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

      auto add_global = [&]()
      {
        const Type& t = _symbol_table.get(s, 0);
        if (!t.is_error()) {
          bool exported = _metadata.has(EXPORT_GLOBAL);
          _globals_output.emplace(s, t.external(exported));
        }
      };

      if (use_function_immediate_assign_hack(node)) {
        // Symbol has already been added by immediate-name-assign recursion
        // hack. But, we may need to make it non-const and enter it in the
        // global symbol table.
        std::size_t index = inside_function() * (_symbol_table.size() - 1);
        _symbol_table.get(s, index).set_const(node.type == Node::ASSIGN_CONST);
        if (!inside_function()) {
          add_global();
        }
        return results[1];
      }

      Type t = results[1];
      t.set_const(node.type == Node::ASSIGN_CONST);

      // Within global blocks, use the top-level symbol table frame.
      if (!inside_function()) {
        add_symbol_checking_collision(node, s, 0, t);
        // Store global in the global map for future use.
        add_global();
        return results[1];
      }

      add_symbol_checking_collision(node, s, t);
      return results[1];
    }

    case Node::INT_CAST:
      if (!results[0].is_float()) {
        error(node, s + " applied to " + rs[0]);
      }
      return Type(Type::INT, results[0].count());

    case Node::FLOAT_CAST:
      if (!results[0].is_int()) {
        error(node, s + " applied to " + rs[0]);
      }
      return Type(Type::FLOAT, results[0].count());

    case Node::VECTOR_CONSTRUCT:
    {
      Type t = results[0];
      std::string ts;
      bool unify_error = false;
      for (std::size_t i = 0; i < results.size(); ++i) {
        if (node.children[i]->type == Node::NAMED_EXPRESSION) {
          error(*node.children[i],
                s + ": named argument in vector construction");
        }
        if (!results[i].primitive()) {
          // Could potentially store the bad types we saw already so as not to
          // repeat the error.
          error(*node.children[i], s + ": element with non-primitive type " +
                                   rs[i] + " in vector construction");
          t = Type::ERROR;
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
      }
      return Type(t.base(), results.size());
    }
    case Node::VECTOR_INDEX:
      if (!results[0].is_vector() || !results[1].is(Type::INT)) {
        error(node, s + " applied to " + rs[0] + " and " + rs[1]);
        return results[0].is_vector() ? results[0].base() : Type::ERROR;
      }
      return results[0].base();

    default:
      error(node, "unimplemented construct");
      return Type::ERROR;
  }
}

void StaticChecker::enter_function(const Type& return_type)
{
  _metadata.add(RETURN_TYPE, return_type);
}

const Type& StaticChecker::current_return_type() const
{
  return _metadata[RETURN_TYPE];
}

bool StaticChecker::inside_function() const
{
  return _metadata.has(RETURN_TYPE);
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
  return _metadata.has(TYPE_EXPR_CONTEXT);
}

bool StaticChecker::use_function_immediate_assign_hack(const Node& node) const
{
  // Node should be type GLOBAL_ASSIGN, ASSIGN_VAR or ASSIGN_CONST.
  return node.children[0]->type == Node::IDENTIFIER &&
      node.children[1]->type == Node::FUNCTION;
}

void StaticChecker::add_symbol_checking_collision(
    const Node& node, const std::string& name, const Type& type)
{
  add_symbol_checking_collision(node, name, _symbol_table.size() - 1, type);
}

void StaticChecker::add_symbol_checking_collision(
    const Node& node, const std::string& name, std::size_t index,
    const Type& type)
{
  if (_symbol_table.has(name, index)) {
    // Skipping on error is debatable as to whether it really skips
    // unnecessary messages, or rather hides real name collisions.
    if (!_symbol_table.get(name, index).is_error()) {
      error(node, (index ? "" : "global ") +
                  ("`" + name + "` redefined"));
    }
    _symbol_table.remove(name, index);
  }
  _symbol_table.add(name, index, type);
}

void StaticChecker::error(const Node& node, const std::string& message)
{
  _errors = true;
  std::string m = message;
  if (_current_function.length()) {
    m = "in function `" + _current_function + "`: " + m;
  }
  std::string full_message = ParseGlobals::error(node.line, node.text, m);
  if (_error_output) {
    *_error_output += full_message + '\n';
  }
  else {
    log_err(full_message);
  }
}

// End namespace yang::internal.
}
}
