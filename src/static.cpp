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

// TODO: is there a sensible more general version of warn_reads which propagates
// tags and ensures every symbol (or even every individual assignment)
// eventually has some external effect (a return, passed to parameter, global
// assignment, depends on it)? Not sure how to deal with closures, and maybe
// it's a bit of a rabbit hole to go down, but it could work.
// Tag propagation could also avoid the extra unwritten-to warning in something
// daft like "var a; a + 1 = a;".
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

Category numeric_type(bool is_float, std::size_t num)
{
  return is_float ?
      (num == 1 ? Type::float_t() : Type::fvec_t(num)) :
      (num == 1 ? Type::int_t() : Type::ivec_t(num));
}

Category binary_type(const Category& a, const Category& b, bool is_float)
{
  if (a.is_error() || b.is_error()) {
    return Category::error();
  }
  std::size_t max = std::max(a.type().vector_size(), b.type().vector_size());
  return numeric_type(is_float, max);
}

// End anonymous namespace.
}

StaticChecker::StaticChecker(
    const ContextInternals& context, ParseData& data,
    std::unordered_map<std::string, Type>& functions_output,
    std::unordered_map<std::string, Global>& globals_output,
    std::unordered_map<std::string, Global>& globals_internal)
  : _context(context)
  , _data(data)
  , _functions_output(functions_output)
  , _globals_output(globals_output)
  , _globals_internal(globals_internal)
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
  bool in_type_context = _scopes.back().metadata.has(TYPE_EXPR_CONTEXT);
  bool context_err = !valid_all_contexts(node) &&
      in_type_context != is_type_expression(node);
  if (context_err) {
    if (!_scopes.back().metadata.has(ERR_EXPR_CONTEXT)) {
      error(node, std::string(in_type_context ? "expected" : "unexpected") +
                  "type in this context");
    }
    // Avoid duplicated errors by adding an override context.
    _scopes.back().metadata.push();
    _scopes.back().metadata.add(ERR_EXPR_CONTEXT, {});
    call_after(node, [=]{_scopes.back().metadata.pop();});
  }
  const Node* no_effect_node = get_no_effect_node(node);
  if (is_tree_root(node) && no_effect_node) {
    // Some nodes need a clearer message about why they "have no effect" than
    // others.
    std::string message =
        node.type == Node::TERNARY ?
        "both branches of conditional operator have no effect" :
        node.type == Node::POSTFIX_INCREMENT ?
        "value of postfix increment is not used (suggest prefix for clarity)" :
        node.type == Node::POSTFIX_DECREMENT ?
        "value of postfix decrement is not used (suggest prefix for clarity)" :
        "operation has no effect";
    error(*no_effect_node, message, false);
  }

  // To make the error messages useful, the general idea here is to fall back to
  // the ERROR type only when the operands (up to errors) do not uniquely
  // determine some result type.
  // For example, the erroneous (1, 1) + (1, 1, 1) results in an ERROR type,
  // since there's no way to decide if an int2 or int3 was intended. However,
  // the erroneous 1 == 1. gives type INT, as the result would be INT whether or
  // not the operand type was intended to be int or float.
  std::function<Category(const result_list&)> result_macro;
#define RESULT [=,&node](const result_list& results) -> Category
#define LEAF [=,&node](const result_list&) -> Category
#define FOR_ANY(condition) if (condition) result_macro =
#define FOR(node_type) FOR_ANY(node.type == Node::node_type)

  // For simple context-insensitive nodes, we define the behaviour with macros.
  FOR(TYPE_VOID) LEAF {return {};};
  FOR(TYPE_INT) LEAF {return numeric_type(false, node.int_value);};
  FOR(TYPE_FLOAT) LEAF {return numeric_type(true, node.int_value);};
  FOR(NAMED_EXPRESSION) RESULT {return results[0];};
  FOR(EXPR_STMT) RESULT {
    // Explicitly treat bare identifier expression as a read.
    if (node.children[0]->type == Node::IDENTIFIER) {
      load(results[0]);
    }
    return Category::error();
  };
  FOR(RETURN_STMT) RESULT {
    // If we're not in a function, we must be in a global block.
    if (!_scopes.back().metadata.has(RETURN_TYPE)) {
      error(node, "return statement inside `global`");
      return Category::error();
    }
    Category t = node.children.empty() ? Category() : load(results[0]);
    Category return_type = _scopes.back().metadata[RETURN_TYPE];
    if (!t.is(return_type)) {
      const auto& n = node.children.empty() ? node : *node.children[0];
      error(n, "returning " + str(t) + " from " +
               str(return_type) + " function");
    }
    return return_type;
  };
  FOR(MEMBER_SELECTION) RESULT {
    auto t = load(results[0]);
    if (!t.user_type()) {
      error(node, "member function access on " + str(results[0]));
      return Category::error();
    }
    if (t.is_error()) {
      return Category::error();
    }

    const auto& m = _context.member_lookup(t.type(), node.string_value);
    if (m.type.is_void()) {
      error(node, "undeclared member function `" +
                  t.type().string(_context) + "::" + node.string_value + "`");
      return Category::error();
    }
    // Omit the first argument (self). Unfortunately, the indirection here
    // makes errors when calling the returned function somewhat vague.
    std::vector<Type> args;
    for (std::size_t i = 1; i < m.type.function_num_args(); ++i) {
      args.push_back(m.type.function_arg(i));
    }
    return Type::function_t(m.type.function_return(), args);
  };
  FOR_ANY(node.type == Node::BREAK_STMT ||
          node.type == Node::CONTINUE_STMT) LEAF {
    if (!_scopes.back().metadata.has(LOOP_BODY)) {
      error(node, str(node) + " outside of loop body");
    }
    return Category::error();
  };
  FOR(IDENTIFIER) LEAF {
    // Look up user types in a type-context.
    if (_scopes.back().metadata.has(TYPE_EXPR_CONTEXT)) {
      const auto& t = _context.type_lookup(node.string_value);
      if (!t.is_void()) {
        return t;
      }
      error(node, "undeclared type `" + node.string_value + "`");
      return Category::error();
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
          std::vector<Type> args;
          for (std::size_t i = 0;
               i < t.ctor.type.function_num_args(); ++i) {
            args.push_back(t.ctor.type.function_arg(i));
          }
          Category r = Type::function_t(
              Type::managed_user_t(t.ctor.type.function_return()), args);
          return r.make_const(true).make_lvalue(true);
        }

        const auto& f = _context.function_lookup(node.string_value);
        if (!f.type.is_void()) {
          return Category(f.type).make_const(true).make_lvalue(true);
        }

        if (!_context.type_lookup(node.string_value).is_void()) {
          error(node, "unexpected typename `" + node.string_value + "`");
        }
        else {
          error(node, "undeclared identifier `" + node.string_value + "`");
        }
        add_symbol(node, node.string_value, Category::error(), false);
        it = _scopes.rbegin();
        break;
      }
    }

    auto& symbol = *it->symbol_table[node.string_value];
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
          symbol.category.type());
    }
    return symbol.category.make_lvalue(true).add_tag(&symbol);
  };
  FOR(EMPTY_EXPR) LEAF {return Type::int_t();};
  FOR(INT_LITERAL) LEAF {return Type::int_t();};
  FOR(FLOAT_LITERAL) LEAF {return Type::float_t();};
  FOR(STRING_LITERAL) LEAF {return Type::managed_user_t<const char>();};
  FOR_ANY(node.type == Node::POW || node.type == Node::MOD ||
          node.type == Node::ADD || node.type == Node::SUB ||
          node.type == Node::MUL || node.type == Node::DIV) RESULT {
    // Takes two integers or floats and produces a value of the same type,
    // with vectorisation.
    auto left = load(results[0]);
    auto right = load(results[1]);
    if (!left.is_binary_match(right) ||
        (!(left.is_int() && right.is_int()) &&
         !(left.is_float() && right.is_float()))) {
      error(node, str(node) + " applied to " +
                  str(left) + " and " + str(right));
      return Category::error();
    }
    return binary_type(left, right, left.is_float());
  };
  FOR_ANY(node.type == Node::EQ || node.type == Node::NE ||
          node.type == Node::GE || node.type == Node::LE ||
          node.type == Node::GT || node.type == Node::LT) RESULT {
    // Takes two integers or floats and produces an integer, with
    // vectorisation.
    auto left = load(results[0]);
    auto right = load(results[1]);
    if (!left.is_binary_match(right)) {
      error(node, str(node) + " applied to " +
                  str(left) + " and " + str(right));
      return Category::error();
    }
    else if (!(left.is_int() && right.is_int()) &&
             !(left.is_float() && right.is_float())) {
      error(node, str(node) + " applied to " +
                  str(left) + " and " + str(right));
    }
    return binary_type(left, right, false);
  };
  FOR_ANY(node.type == Node::FOLD_LOGICAL_OR ||
          node.type == Node::FOLD_LOGICAL_AND ||
          node.type == Node::FOLD_BITWISE_OR ||
          node.type == Node::FOLD_BITWISE_AND ||
          node.type == Node::FOLD_BITWISE_XOR ||
          node.type == Node::FOLD_BITWISE_LSHIFT ||
          node.type == Node::FOLD_BITWISE_RSHIFT) RESULT {
    auto t = load(results[0]);
    if (!t.is_vector() || !t.is_int()) {
      error(node, str(node) + " applied to " + str(t));
    }
    return Type::int_t();
  };
  FOR_ANY(node.type == Node::FOLD_POW || node.type == Node::FOLD_MOD ||
          node.type == Node::FOLD_ADD || node.type == Node::FOLD_SUB ||
          node.type == Node::FOLD_MUL || node.type == Node::FOLD_DIV) RESULT {
    auto t = load(results[0]);
    if (!t.is_vector() || !(t.is_int() || t.is_float())) {
      error(node, str(node) + " applied to " + str(t));
      return Category::error();
    }
    return t.is_int() ? Type::int_t() : Type::float_t();
  };
  FOR_ANY(node.type == Node::FOLD_EQ || node.type == Node::FOLD_NE ||
          node.type == Node::FOLD_GE || node.type == Node::FOLD_LE ||
          node.type == Node::FOLD_GT || node.type == Node::FOLD_LT) RESULT {
    auto t = load(results[0]);
    if (!t.is_vector() || !(t.is_int() || t.is_float())) {
      error(node, str(node) + " applied to " + str(t));
    }
    return Type::int_t();
  };
  FOR_ANY(node.type == Node::LOGICAL_NEGATION ||
          node.type == Node::BITWISE_NEGATION) RESULT {
    auto t = load(results[0]);
    if (!t.is_int()) {
      error(node, str(node) + " applied to " + str(t));
    }
    return numeric_type(false, t.type().vector_size());
  };
  FOR(ARITHMETIC_NEGATION) RESULT {
    auto t = load(results[0]);
    if (!(t.is_int() || t.is_float())) {
      error(node, str(node) + " applied to " + str(t));
      return Category::error();
    }
    return t;
  };
  FOR_ANY(node.type == Node::INCREMENT || node.type == Node::DECREMENT ||
          node.type == Node::POSTFIX_INCREMENT ||
          node.type == Node::POSTFIX_DECREMENT) RESULT {
    Category t = results[0];
    for (void* tag : t.tags()) {
      ((symbol_t*)tag)->warn_writes = false;
    }

    if (!(t.is_int() || t.is_float())) {
      error(node, str(node) + " applied to " + str(t));
      return Category::error().add_tags(t);
    }
    if (!t.is_lvalue()) {
      error(node, str(node) + " applied to non-lvalue");
    }
    else if (!t.not_const()) {
      error(node, str(node) + " applied to constant");
    }
    return node.type == Node::INCREMENT || node.type == Node::DECREMENT ?
        t.make_const(false).make_lvalue(true) : load(t);
  };
  FOR_ANY(
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
      node.type == Node::ASSIGN_MUL || node.type == Node::ASSIGN_DIV) RESULT {
    auto left = results[0];
    auto right = load(results[1]);
    bool math =
        node.type == Node::ASSIGN_POW || node.type == Node::ASSIGN_MOD ||
        node.type == Node::ASSIGN_ADD || node.type == Node::ASSIGN_SUB ||
        node.type == Node::ASSIGN_MUL || node.type == Node::ASSIGN_DIV;
    for (void* tag : left.tags()) {
      auto sym = (symbol_t*)tag;
      sym->warn_writes = false;
    }

    Category c = Type::void_t();
    if (node.type != Node::ASSIGN) {
      if (!left.is_assign_binary_match(right)) {
        c = Category::error().add_tags(left);
      }
      else if (!math && !(left.is_int() && right.is_int())) {
        c = binary_type(left, right, false).make_lvalue(true).add_tags(left);
      }
      else if (math && !(left.is_int() && right.is_int()) &&
               !(left.is_float() && right.is_float())) {
        c = Category::error().add_tags(left);
      }
    }
    else if (!left.is(right)) {
      c = Category::error().add_tags(left);
    }
    if (c.not_void()) {
      error(node, str(node) + " applied to " +
                  str(left) + " and " + str(right));
      return c;
    }

    if (!left.is_lvalue()) {
      error(node, "assignment to non-lvalue");
    }
    else if (!left.not_const()) {
      error(node, "assignment to constant");
    }
    return left.make_const(false).make_lvalue(true);
  };
  FOR(INT_CAST) RESULT {
    auto t = load(results[0]);
    if (!t.is_float()) {
      error(node, str(node) + " applied to " + str(t));
    }
    return numeric_type(false, t.type().vector_size());
  };
  FOR(FLOAT_CAST) RESULT {
    auto t = load(results[0]);
    if (!t.is_int()) {
      error(node, str(node) + " applied to " + str(t));
    }
    return numeric_type(true, t.type().vector_size());
  };
  FOR(VECTOR_CONSTRUCT) RESULT {
    Category t = load(results[0]);
    std::string ts;
    bool unify_error = false;
    for (std::size_t i = 0; i < results.size(); ++i) {
      if (node.children[i]->type == Node::NAMED_EXPRESSION) {
        error(*node.children[i], "named vector element");
      }
      Category u = load(results[i]);
      if (!u.is(Type::int_t()) && !u.is(Type::float_t())) {
        error(*node.children[i],
              "vector element with non-primitive type " + str(u));
        t = Category::error();
      }
      if (i) {
        bool error = t.is_error();
        t = t.unify(u);
        if (!error && !u.is_error() && t.is_error()) {
          unify_error = true;
        }
        ts += ", ";
      }
      ts += str(u);
    }
    if (unify_error) {
      error(node, "differing types " + ts + " in vector");
      return Category::error();
    }
    return numeric_type(t.is_float(), results.size());
  };
  FOR(VECTOR_INDEX) RESULT {
    auto left = results[0];
    for (std::size_t i = 1; i < results.size(); ++i) {
      auto index = load(results[i]);
      if (!index.is(Type::int_t())) {
        error(*node.children[i], "type " + str(index) + " in vector index");
      }
    }
    if (!left.is_vector()) {
      error(node, "type " + str(left) + " cannot be indexed");
      return Category::error().add_tags(left);
    }
    auto t = left.vector_element(results.size() - 1);
    return results.size() == 2 ? t : load(t);
  };

#undef FOR
#undef FOR_ANY
  if (result_macro) {
    result(node, result_macro);
  }

  switch (node.type) {
    case Node::TYPE_FUNCTION:
      type_function:
      result(node, RESULT
      {
        bool err = results[0].is_error();
        std::vector<Type> args;
        for (std::size_t i = 1; i < results.size(); ++i) {
          if (!results[i].not_void()) {
            error(*node.children[i], "function type with `void` argument type");
          }
          if (results[i].is_error()) {
            err = true;
          }
          args.push_back(results[i].type());
        }
        return err ? Category::error() :
            Type::function_t(results[0].type(), args);
      });
      break;

    case Node::PROGRAM:
      // Make sure to warn on unused top-level elements. This doesn't actually
      // pop anything, since it's the last frame.
      call_after(node, [=]{warn_unreferenced_variables();});
      result(node, LEAF {return {};});
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
      call_after(node, [=]
      {
        pop_symbol_tables();
        _scopes.pop_back();
      });
      result(node, LEAF {return {};});
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
      call_after(*node.children[0], [=]{pop_symbol_tables();});
      if (node.children[0]->type == Node::IDENTIFIER) {
        add_symbol(node, node.children[0]->string_value, {}, false, false);
      }

      result(node, RESULT {
        Category t =
            load(results[1]).make_const(node.type == Node::ASSIGN_CONST ||
                                        node.type == Node::GLOBAL_ASSIGN);

        if (node.type == Node::GLOBAL_ASSIGN && !t.function()) {
          error(node, "global assignment of type " + str(results[1]));
          t = Category::error();
        }
        if (node.type != Node::GLOBAL_ASSIGN && !t.not_void()) {
          error(node, "assignment of type " + str(results[1]));
          t = Category::error();
        }
        if (node.children[0]->type != Node::IDENTIFIER) {
          error(node, "expected identifer on declaration LHS");
          return t.make_lvalue(true);
        }
        const std::string& s = node.children[0]->string_value;

        bool global_scope = node.type == Node::GLOBAL_ASSIGN ||
            (_scopes.back().symbol_table.size() <= 3 &&
             !_scopes.back().metadata.has(RETURN_TYPE) &&
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
            (exported ? _globals_output : _globals_internal).emplace(
                s, Global(t.type(), !t.not_const()));
          }
          if (exported) {
            sym[s]->warn_writes = sym[s]->warn_reads = false;
          }
        }

        // Merge warnings for immediate assign hack.
        if (use_function_immediate_assign_hack(node)) {
          sym[s]->warn_reads &= _immediate_left_assign_warn_reads;
        }
        sym[s]->closed = sym[s]->warn_closed =
            node.int_value & Node::MODIFIER_CLOSED;
        if (global_scope && sym[s]->closed) {
          sym[s]->warn_closed = false;
          error(node, "`closed` modifier has no effect on global " +
                      node.string_value, false);
        }

        // Only export functions get added to the function table. They also
        // are automatically assumed to be referenced.
        if (node.type == Node::GLOBAL_ASSIGN && !t.is_error() &&
            node.int_value & Node::MODIFIER_EXPORT) {
          _functions_output.emplace(s, t.type());
          sym[s]->warn_reads = false;
        }
        return t.make_lvalue(true).add_tag(sym[s]);
      });
      break;

    case Node::FUNCTION:
      _scopes.back().metadata.push();
      _scopes.back().metadata.add(TYPE_EXPR_CONTEXT, {});
      result(node, RESULT {
        return results[0].function() ? results[0] : Category::error();
      });

      call_after_result(*node.children[0], [=,&node](const Category& result)
      {
        // Erase type context.
        _scopes.back().metadata.pop();
        // Make sure it's const so functions can't set themselves to different
        // values inside the body. Grammar ensures this will be a function type.
        Category t = result.make_const(true);

        // Functions need two symbol table frames: one for recursive hack, one
        // for the arguments, and one for the body.
        _scopes.emplace_back(node, _immediate_left_assign.length() ?
                                   _immediate_left_assign : "<anon>");
        push_symbol_tables();
        node.static_info.scope_number = _scopes.back().scope_numbering.back();
        if (_immediate_left_assign.length()) {
          // Immediate left assigns are implicitly closed.
          add_symbol(node, _immediate_left_assign, t, false);
          _scopes.back().symbol_table[_immediate_left_assign]->closed = true;
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
            Category u = elem ? t.type().function_arg(elem - 1) :
                                t.type().function_return();
            add_symbol(*ptr, name, u.make_const(true), false);
            // TODO: arguments are implicitly closed so far, since it's a bit
            // tricky to allow closed on arglists in the parser. Fix that maybe.
            _scopes.back().symbol_table[name]->closed = true;
            arg_names.insert(name);
            ++elem;
          }
        }
        _scopes.back().metadata.add(RETURN_TYPE, t.type().function_return());
        push_symbol_tables();
      });

      call_after(node, [=,&node](const result_list& results)
      {
        if (!_scopes.back().metadata[RETURN_TYPE].is_void() &&
            results[1].is_void()) {
          error(node, "not all code paths return a value");
        }
        pop_symbol_tables();
        pop_symbol_tables();
        // The last symbol table contains only the recursive hack: we just want
        // to merge its unreferenced warning information with the symbol about
        // to be added in the enclosing scope.
        std::vector<std::pair<std::string, symbol_t*>> symbols;
        _scopes.back().symbol_table.get_symbols(symbols, 0, 2);
        for (const auto& pair : symbols) {
          _immediate_left_assign_warn_reads = pair.second->warn_reads;
          pair.second->warn_reads = false;
        }

        // Symbol reference warnings.
        warn_unreferenced_variables();
        _scopes.pop_back();
      });
      break;

    case Node::LOOP_AFTER_BLOCK:
    case Node::BLOCK:
      push_symbol_tables();
      call_after(node, [=]{pop_symbol_tables();});
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
        for (const Category& t : results) {
          if (!t.is_error()) {
            return t;
          }
        }
        return Category::error();
      });
      break;

    case Node::IF_STMT:
      push_symbol_tables();
      call_after(node, [=]{pop_symbol_tables();});
      result(node, RESULT {
        auto t = load(results[0]);
        if (!t.is(Type::int_t())) {
          error(*node.children[0],
                str(node) + " branching on " + str(t));
        }
        auto& body = results.size() > 2 ? node.children[2] : node.children[1];
        if (body->type == Node::EXPR_STMT &&
            body->children[0]->type == Node::EMPTY_EXPR) {
          error(*body, "empty statement in " + str(node), false);
        }
        // An IF_STMT definitely returns a value only if both branches
        // definitely return a value.
        Category c_else = results.size() > 2 ? results[2] : Category::error();
        return results[1].unify(c_else);
      });
      break;

    case Node::FOR_STMT:
    case Node::WHILE_STMT:
    case Node::DO_WHILE_STMT:
      push_symbol_tables();
      call_after(node, [=]{pop_symbol_tables();});
      // Insert a marker into the symbol table that break and continue
      // statements can check for.
      _scopes.back().metadata.add(LOOP_BODY, {});
      // Separate scope for the loop after-statement.
      if (node.type == Node::FOR_STMT) {
        call_after(*node.children[1], [=]{_scopes.back().metadata.push();});
        call_after(*node.children[2], [=]{_scopes.back().metadata.pop();});
      }
      result(node, RESULT {
        std::size_t cond = node.type != Node::WHILE_STMT;
        if (!load(results[cond]).is(Type::int_t())) {
          error(*node.children[cond],
                str(node) + " branching on " + str(results[cond]));
        }
        // Do-while loops run at least once, but would have to consider break
        // and continue statements to warn about definitely-dead code.
        return Category::error();
      });
      break;

    case Node::TERNARY:
      call_after_result(*node.children[0], [=,&node](const Category& result)
      {
        if (!result.is_vector()) {
          push_symbol_tables();
          call_after(*node.children[1], [=]{
            pop_symbol_tables();
            push_symbol_tables();
          });
          call_after(*node.children[2], [=]{pop_symbol_tables();});
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
        auto cond = load(results[0]);
        if (!results[1].is(results[2])) {
          error(node, str(node) + " applied to " +
                      str(results[1]) + " and " + str(results[2]));
          err = true;
        }
        if (!cond.is_int()) {
          error(node, str(node) + " branching on " + str(cond));
          err = true;
        }

        if (!err && cond.is_vector() &&
            (!results[1].is_vector() || cond.type().vector_size() !=
                                        results[1].type().vector_size())) {
          error(node, std::to_string(cond.type().vector_size()) +
                      "-vectorised " + str(node) + " applied to " +
                      str(results[1]) + " and " + str(results[2]));
        }
        Category t = results[1].unify(results[2]);
        if (!t.is_lvalue()) {
          load(results[1]);
          load(results[2]);
        }
        return t;
      });
      break;

    case Node::CALL:
      if (_scopes.back().metadata.has(TYPE_EXPR_CONTEXT)) {
        goto type_function;
      }
      result(node, RESULT {
        auto t = load(results[0]);
        for (std::size_t i = 1; i < results.size(); ++i) {
          load(results[i]);
        }
        // The grammar doesn't distinguish function-type construction from
        // call-expressions in all contexts, so we need to check here.
        for (std::size_t i = 1; i < results.size(); ++i) {
          if (node.children[i]->type == Node::NAMED_EXPRESSION) {
            error(*node.children[i],
                  str(node) + ": named argument in function call");
          }
        }
        if (!t.function()) {
          error(node, str(node) + " applied to " + str(t));
          return Category::error();
        }
        if (!t.element_size(results.size())) {
          error(node, str(t) + " called with " +
                      std::to_string(results.size() - 1) + " argument(s)");
        }
        else {
          for (std::size_t i = 1; i < results.size(); ++i) {
            auto u = load(results[i]);
            if (!t.element_is(i, u)) {
              error(*node.children[i],
                    str(t) + " called with " + str(u) +
                    " in position " + std::to_string(i - 1));
            }
          }
        }
        return t.is_error() ? Category::error() : t.type().function_return();
      });
      break;

    case Node::LOGICAL_OR:
    case Node::LOGICAL_AND:
      // Right-hand-sides of non-vectorised logical operators need an extra
      // scope, as they won't always be run.
      call_after_result(*node.children[0], [=,&node](const Category& result)
      {
        if (!result.is_vector()) {
          push_symbol_tables();
          call_after(*node.children[1], [=]{pop_symbol_tables();});
        }
      });
    case Node::BITWISE_OR:
    case Node::BITWISE_AND:
    case Node::BITWISE_XOR:
    case Node::BITWISE_LSHIFT:
    case Node::BITWISE_RSHIFT:
      result(node, RESULT {
        auto left = load(results[0]);
        auto right = load(results[1]);
        // Takes two integers and produces an integer, with vectorisation.
        if (!left.is_binary_match(right)) {
          error(node, str(node) + " applied to " +
                      str(left) + " and " + str(right));
          return Category::error();
        }
        if (!left.is_int() || !right.is_int()) {
          error(node, str(node) + " applied to " +
                      str(left) + " and " + str(right));
        }
        return binary_type(left, right, false);
      });
      break;

    default: {}
  }

  // Type/expression context error result overrides any other result.
  if (context_err) {
    result(node, LEAF {return Category::error();});
  }
#undef LEAF
#undef RESULT
}

Category StaticChecker::after(const Node& node, const result_list&)
{
  error(node, "unimplemented construct");
  return Category::error();
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
  _scopes.back().symbol_table.pop();
}

void StaticChecker::warn_unreferenced_variables()
{
  for (const auto& sym : _scopes.back().symbols) {
    if (sym.warn_writes && sym.warn_reads) {
      error(*sym.declaration,
            "symbol `" + sym.name + "` is never referenced", false);
    }
    else if (sym.warn_closed) {
      error(*sym.declaration,
            "symbol `" + sym.name + "` is never closed over", false);
    }
    else if (sym.warn_writes) {
      error(*sym.declaration,
            "symbol `" + sym.name + "` is never written to", false);
    }
    else if (sym.warn_reads) {
      error(*sym.declaration,
            "symbol `" + sym.name + "` is never read", false);
    }
  }
}

void StaticChecker::add_symbol(
    const Node& node, const std::string& name, const Category& category,
    bool global, bool unreferenced_warning)
{
  auto& scope = global ? _scopes[0] : _scopes.back();
  scope.symbols.emplace_back();
  auto& sym = scope.symbols.back();

  sym.category = category;
  sym.scope_number = global ? 0 : _scopes.back().scope_numbering.back();
  sym.declaration = &node;
  sym.name = name;
  // We don't care if there are no writes to a const symbol, so we just set
  // warn_writes to false.
  unreferenced_warning = unreferenced_warning && !category.is_error();
  sym.warn_writes = unreferenced_warning && category.not_const();
  sym.warn_reads = unreferenced_warning;
  scope.symbol_table.add(name, &sym);
}

void StaticChecker::add_symbol_checking_collision(
    const Node& node, const std::string& name, const Category& category,
    bool global, bool unreferenced_warning)
{
  auto& table = global ? _scopes[0].symbol_table : _scopes.back().symbol_table;
  if (table.has(name, table.size() - 1)) {
    // Skipping on error is debatable as to whether it really skips
    // unnecessary messages, or rather hides real name collisions.
    if (!table.get(name, table.size() - 1)->category.is_error()) {
      error(node, (global ? "global " : "symbol ") +
                  ("`" + name + "` redefined"));
    }
    table.remove(name, table.size() - 1);
  }
  add_symbol(node, name, category, global, unreferenced_warning);
}

Category StaticChecker::load(const Category& a)
{
  if (!a.is_lvalue()) {
    return a;
  }
  for (void* tag : a.tags()) {
    auto sym = (symbol_t*)tag;
    sym->warn_reads = false;
  }
  return a.is_error() ? a : a.type();
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

std::string StaticChecker::str(const Category& category) const
{
  return category.is_error() ? "<error>" :
      "`" + category.type().string(_context) + "`";
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
  , symbol_table(nullptr)
  , scope_numbering{0}
  , scope_numbering_next(1)
{
}

}} // ::yang::internal
