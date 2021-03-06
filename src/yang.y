/*============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================*/
%{

#include "../src/ast.h"
#include "yang.y.h"
#include "yang.l.h"

typedef yang::internal::Node Node;
typedef yang::internal::ParseData ParseData;

int yang_error(yyscan_t scan, const char* message, bool error = true)
{
  ParseData* data = (ParseData*)yang_get_extra(scan);
  std::size_t left = data->character - yang_get_leng(scan);
  std::size_t right = data->character;
  (error ? data->errors : data->warnings).push_back(
      data->format_error(left, right, left, right, message, error));
  return 0;
}

%}

%pure-parser
/* BYACC chops off last character of parse parameter name. */
%lex-param {yyscan_t scan_}
%parse-param {yyscan_t scan_}
%union {
  yang::internal::Node* node;
}

  /* Tokens. */

%token <node> T_EOF 0
%token <node> '(' ')' '{' '}' '[' ']' '.' ',' ';'

%token <node> T_INTERFACE
%token <node> T_GLOBAL
%token <node> T_EXPORT
%token <node> T_CLOSED
%token <node> T_VAR
%token <node> T_CONST
%token <node> T_IF
%token <node> T_ELSE
%token <node> T_FOR
%token <node> T_WHILE
%token <node> T_DO
%token <node> T_BREAK
%token <node> T_CONTINUE
%token <node> T_RETURN

%token <node> T_TERNARY_L
%token <node> T_TERNARY_R
%token <node> T_LOGICAL_NEGATION
%token <node> T_LOGICAL_OR
%token <node> T_LOGICAL_AND
%token <node> T_BITWISE_NEGATION
%token <node> T_BITWISE_OR
%token <node> T_BITWISE_AND
%token <node> T_BITWISE_XOR
%token <node> T_BITWISE_LSHIFT
%token <node> T_BITWISE_RSHIFT
%token <node> T_POW
%token <node> T_MOD
%token <node> T_ADD
%token <node> T_SUB
%token <node> T_MUL
%token <node> T_DIV
%token <node> T_EQ
%token <node> T_NE
%token <node> T_GE
%token <node> T_LE
%token <node> T_GT
%token <node> T_LT
%token <node> T_FOLD
%token <node> T_SCOPE_RESOLUTION
%token <node> T_ASSIGN
%token <node> T_ASSIGN_LOGICAL_OR
%token <node> T_ASSIGN_LOGICAL_AND
%token <node> T_ASSIGN_BITWISE_OR
%token <node> T_ASSIGN_BITWISE_AND
%token <node> T_ASSIGN_BITWISE_XOR
%token <node> T_ASSIGN_BITWISE_LSHIFT
%token <node> T_ASSIGN_BITWISE_RSHIFT
%token <node> T_ASSIGN_POW
%token <node> T_ASSIGN_MOD
%token <node> T_ASSIGN_ADD
%token <node> T_ASSIGN_SUB
%token <node> T_ASSIGN_MUL
%token <node> T_ASSIGN_DIV
%token <node> T_INCREMENT
%token <node> T_DECREMENT
%token <node> T_IDENTIFIER
%token <node> T_INT_LITERAL
%token <node> T_FLOAT_LITERAL
%token <node> T_STRING_LITERAL
%token <node> T_TYPE_LITERAL

  /* Operator precedence. */

%left P_ELEM
%nonassoc T_IF
%nonassoc T_ELSE
%right T_TERNARY_L T_TERNARY_R
    T_ASSIGN T_ASSIGN_LOGICAL_OR T_ASSIGN_LOGICAL_AND
    T_ASSIGN_BITWISE_OR T_ASSIGN_BITWISE_AND T_ASSIGN_BITWISE_XOR
    T_ASSIGN_BITWISE_LSHIFT T_ASSIGN_BITWISE_RSHIFT
    T_ASSIGN_POW T_ASSIGN_MOD
    T_ASSIGN_ADD T_ASSIGN_SUB T_ASSIGN_MUL T_ASSIGN_DIV
%left T_FOLD
%left T_LOGICAL_OR
%left T_LOGICAL_AND
%left T_EQ T_NE
%left T_GE T_LE T_GT T_LT
%left T_BITWISE_OR
%left T_BITWISE_XOR
%left T_BITWISE_AND
%left T_BITWISE_LSHIFT T_BITWISE_RSHIFT
%left T_ADD T_SUB
%left T_MUL T_DIV T_MOD
%right P_UNARY_L
%right T_POW
%left '.' '[' '(' T_INCREMENT T_DECREMENT
%left T_STRING_LITERAL
  /* Precedence of '.' and '::' for member selection. */
%left T_IDENTIFIER T_SCOPE_RESOLUTION

  /* Types. */

%type <node> program
%type <node> elem_list
%type <node> elem
%type <node> interface
%type <node> member_list
%type <node> member
%type <node> opt_export
%type <node> opt_negation
%type <node> opt_closed
%type <node> stmt_list
%type <node> stmt
%type <node> opt_expr
%type <node> opt_identifier
%type <node> expr_named
%type <node> expr_list
%type <node> expr_functional
%type <node> expr
%type <node> string_literal
%type <node> identifier
%start program

%%

  /* Language grammar. */

program
  : elem_list T_EOF
{$$ = ((ParseData*)yang_get_extra(scan))->parser_output = $1;
 $$->type = Node::PROGRAM;}
  ;

elem_list
  :
{$$ = new Node(scan, Node::ERROR);}
  | elem_list elem
{$$ = $1;
 if ($2) {
   $$->add($2);
 }}
  ;

elem
  : interface
{$$ = $1;}
  | opt_export opt_negation T_GLOBAL stmt
{if ($4->type != Node::BLOCK) {
   // Ensure a consistent scope depth for all global declarations.
   $4 = new Node(scan, $3, Node::BLOCK, $4);
 }
 $$ = new Node(scan, $3, Node::GLOBAL, $4);
 $$->int_value |= $1->int_value | $2->int_value;
 $$->extend_bounds($1);
 $$->extend_inner_bounds($2);}
  | opt_export expr T_ASSIGN expr
{$$ = new Node(scan, $3, Node::GLOBAL_ASSIGN, $2, $4);
 $$->int_value |= $1->int_value;
 $$->extend_bounds($1);}
  | ';'
{$$ = nullptr;}
  ;

interface
  : T_INTERFACE T_IDENTIFIER '{' member_list '}'
{$$ = $4;
 $$->type = Node::INTERFACE;
 $$->clear_bounds($2);
 $$->string_value = $2->string_value;
 $$->extend_bounds($1);}
  ;

member_list
  :
{$$ = new Node(scan, Node::ERROR);}
  | member_list member ';'
{$$ = $1;
 $$->add($2);}
  ;

member
  : expr T_IDENTIFIER
{$$ = new Node(scan, $2, Node::INTERFACE_MEMBER, $1);
 $$->string_value = $2->string_value;}
  ;

opt_export
  : T_EXPORT
{$$ = new Node(scan, Node::INT_LITERAL, Node::MODIFIER_EXPORT);}
  |
{$$ = new Node(scan, Node::INT_LITERAL);}
  ;

opt_negation
  : T_BITWISE_NEGATION
{$$ = new Node(scan, Node::INT_LITERAL, Node::MODIFIER_NEGATION);}
  |
{$$ = new Node(scan, Node::INT_LITERAL);}
  ;

opt_closed
  : T_CLOSED
{$$ = new Node(scan, Node::INT_LITERAL, Node::MODIFIER_CLOSED);}
  |
{$$ = new Node(scan, Node::INT_LITERAL);}
  ;

stmt_list
  :
{$$ = new Node(scan, Node::ERROR);}
  | stmt_list stmt
{$$ = $1;
 $$->add($2);}
  ;

stmt
  : opt_expr ';'
{$$ = new Node(scan, $1, Node::EXPR_STMT, $1);
 $$->extend_inner_bounds($2);}
  | T_RETURN opt_expr ';'
{$$ = new Node(scan, $1, Node::RETURN_STMT);
 if ($2->type != Node::EMPTY_EXPR) {
   $$->add($2);
 }
 $$->extend_inner_bounds($3);}
  | T_IF '(' expr ')' stmt %prec T_IF
{$$ = new Node(scan, $1, Node::IF_STMT, $3, $5);}
  | T_IF '(' expr ')' stmt T_ELSE stmt
{$$ = new Node(scan, $1, Node::IF_STMT, $3, $5, $7);}
  | T_FOR '(' opt_expr ';' opt_expr ';' opt_expr ')' stmt
{$$ = new Node(scan, $1, Node::FOR_STMT,
               $3, $5, new Node(scan, $7, Node::LOOP_AFTER_BLOCK, $7));
 $$->add($9);}
  | T_WHILE '(' expr ')' stmt
{$$ = new Node(scan, $1, Node::WHILE_STMT, $3, $5);}
  | T_DO stmt T_WHILE '(' expr ')' ';'
{$$ = new Node(scan, $1, Node::DO_WHILE_STMT, $2, $5);
 $$->extend_bounds($7);}
  | T_BREAK ';'
{$$ = new Node(scan, $1, Node::BREAK_STMT);
 $$->extend_inner_bounds($2);}
  | T_CONTINUE ';'
{$$ = new Node(scan, $1, Node::CONTINUE_STMT);
 $$->extend_inner_bounds($2);}
  | '{' stmt_list '}'
{$$ = $2;
 $$->type = Node::BLOCK;
 $$->extend_bounds($1);
 $$->extend_bounds($3);}
  ;

opt_expr
  : expr
{$$ = $1;}
  |
{$$ = new Node(scan, Node::EMPTY_EXPR);}
  ;

opt_identifier
  : T_IDENTIFIER
{$$ = $1;}
  |
{$$ = new Node(scan, Node::IDENTIFIER, "");}
  ;

expr_named
  : expr opt_identifier
{$$ = $1;
 if (!$2->string_value.empty()) {
   $$ = new Node(scan, $$, Node::NAMED_EXPRESSION, $$);
   $$->string_value = $2->string_value;
   $$->extend_inner_bounds($2);
 }}
  ;

expr_list
  : expr_named
{$$ = new Node(scan, Node::ERROR);
 $$->add($1);}
  | expr_list ',' expr_named
{$$ = $1;
 $$->add($3);}
  ;

expr_functional
  : expr '(' ')'
{$$ = new Node(scan, $1, Node::ERROR, $1);
 $$->set_inner_bounds($2);
 $$->extend_bounds($3);}
  | expr '(' expr_list ')'
{$$ = $3;
 $$->add_front($1);
 $$->set_inner_bounds($2);
 $$->extend_bounds($4);}
  ;

expr
  /* Complicated type-expression interaction. */
  : T_TYPE_LITERAL
{$$ = $1;}
  | identifier
{$$ = $1;}
  | expr_functional '{' stmt_list '}'
{$1->type = Node::TYPE_FUNCTION;
 $3->type = Node::BLOCK;
 $$ = new Node(scan, $1, Node::FUNCTION, $1, $3);
 $$->extend_bounds($4);}
  | expr_functional
{$$ = $1;
 $$->type = Node::CALL;}

  /* Miscellaeneous. */
  | '(' expr ')'
{$$ = $2;
 $$->extend_bounds($1);
 $$->extend_bounds($3);}
  | expr '.' T_IDENTIFIER
{$$ = new Node(scan, $2, Node::MEMBER_SELECTION, $1);
 $$->string_value = $3->string_value;
 $$->extend_bounds($3);}
  | expr T_TERNARY_L expr T_TERNARY_R expr
{$$ = new Node(scan, $2, Node::TERNARY, $1, $3, $5);}
  | T_INT_LITERAL
{$$ = $1;}
  | T_FLOAT_LITERAL
{$$ = $1;}
  /* Precedence needed to disambiguate the (incorrect, regardless) ambiguous
     program: x = "a" "b" = ... */
  | string_literal %prec P_ELEM
{$$ = $1;}

  /* Binary operators. */

  | expr T_LOGICAL_OR expr
{$$ = new Node(scan, $2, Node::LOGICAL_OR, $1, $3);}
  | expr T_LOGICAL_AND expr
{$$ = new Node(scan, $2, Node::LOGICAL_AND, $1, $3);}
  | expr T_BITWISE_OR expr
{$$ = new Node(scan, $2, Node::BITWISE_OR, $1, $3);}
  | expr T_BITWISE_AND expr
{$$ = new Node(scan, $2, Node::BITWISE_AND, $1, $3);}
  | expr T_BITWISE_XOR expr
{$$ = new Node(scan, $2, Node::BITWISE_XOR, $1, $3);}
  | expr T_BITWISE_LSHIFT expr
{$$ = new Node(scan, $2, Node::BITWISE_LSHIFT, $1, $3);}
  | expr T_BITWISE_RSHIFT expr
{$$ = new Node(scan, $2, Node::BITWISE_RSHIFT, $1, $3);}
  | expr T_POW expr
{$$ = new Node(scan, $2, Node::POW, $1, $3);}
  | expr T_MOD expr
{$$ = new Node(scan, $2, Node::MOD, $1, $3);}
  | expr T_ADD expr
{$$ = new Node(scan, $2, Node::ADD, $1, $3);}
  | expr T_SUB expr
{$$ = new Node(scan, $2, Node::SUB, $1, $3);}
  | expr T_MUL expr
{$$ = new Node(scan, $2, Node::MUL, $1, $3);}
  | expr T_DIV expr
{$$ = new Node(scan, $2, Node::DIV, $1, $3);}
  | expr T_EQ expr
{$$ = new Node(scan, $2, Node::EQ, $1, $3);}
  | expr T_NE expr
{$$ = new Node(scan, $2, Node::NE, $1, $3);}
  | expr T_GE expr
{$$ = new Node(scan, $2, Node::GE, $1, $3);}
  | expr T_LE expr
{$$ = new Node(scan, $2, Node::LE, $1, $3);}
  | expr T_GT expr
{$$ = new Node(scan, $2, Node::GT, $1, $3);}
  | expr T_LT expr
{$$ = new Node(scan, $2, Node::LT, $1, $3);}

  /* Fold operators. */

  | T_FOLD T_LOGICAL_OR expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_LOGICAL_OR, $3);}
  | T_FOLD T_LOGICAL_AND expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_LOGICAL_AND, $3);}
  | T_FOLD T_BITWISE_OR expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_BITWISE_OR, $3);}
  | T_FOLD T_BITWISE_AND expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_BITWISE_AND, $3);}
  | T_FOLD T_BITWISE_XOR expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_BITWISE_XOR, $3);}
  | T_FOLD T_BITWISE_LSHIFT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_BITWISE_LSHIFT, $3);}
  | T_FOLD T_BITWISE_RSHIFT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_BITWISE_RSHIFT, $3);}
  | T_FOLD T_POW expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_POW, $3);}
  | T_FOLD T_MOD expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_MOD, $3);}
  | T_FOLD T_ADD expr %prec P_UNARY_L
{$$ = new Node(scan, $1,Node::FOLD_ADD, $3);}
  | T_FOLD T_SUB expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_SUB, $3);}
  | T_FOLD T_MUL expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_MUL, $3);}
  | T_FOLD T_DIV expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_DIV, $3);}
  | T_FOLD T_EQ expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_EQ, $3);}
  | T_FOLD T_NE expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_NE, $3);}
  | T_FOLD T_GE expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_GE, $3);}
  | T_FOLD T_LE expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_LE, $3);}
  | T_FOLD T_GT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_GT, $3);}
  | T_FOLD T_LT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::FOLD_LT, $3);}

  /* Prefix unary operators. */

  | T_LOGICAL_NEGATION expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::LOGICAL_NEGATION, $2);}
  | T_BITWISE_NEGATION expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::BITWISE_NEGATION, $2);}
  | T_SUB expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::ARITHMETIC_NEGATION, $2);}
  | T_ADD expr %prec P_UNARY_L
{$$ = $2;
 $$->extend_bounds($1);}

  /* Assignment operators. */

  | opt_closed T_VAR expr T_ASSIGN expr
{$$ = new Node(scan, $4, Node::ASSIGN_VAR, $3, $5);
 $$->int_value |= $1->int_value;
 $$->extend_bounds($1);}
  | opt_closed T_CONST expr T_ASSIGN expr
{$$ = new Node(scan, $4, Node::ASSIGN_CONST, $3, $5);
 $$->int_value |= $1->int_value;
 $$->extend_bounds($1);}
  | expr T_ASSIGN expr
{$$ = new Node(scan, $2, Node::ASSIGN, $1, $3);}
  | expr T_ASSIGN_LOGICAL_OR expr
{$$ = new Node(scan, $2, Node::ASSIGN_LOGICAL_OR, $1, $3);}
  | expr T_ASSIGN_LOGICAL_AND expr
{$$ = new Node(scan, $2, Node::ASSIGN_LOGICAL_AND, $1, $3);}
  | expr T_ASSIGN_BITWISE_OR expr
{$$ = new Node(scan, $2, Node::ASSIGN_BITWISE_OR, $1, $3);}
  | expr T_ASSIGN_BITWISE_AND expr
{$$ = new Node(scan, $2, Node::ASSIGN_BITWISE_AND, $1, $3);}
  | expr T_ASSIGN_BITWISE_XOR expr
{$$ = new Node(scan, $2, Node::ASSIGN_BITWISE_XOR, $1, $3);}
  | expr T_ASSIGN_BITWISE_LSHIFT expr
{$$ = new Node(scan, $2, Node::ASSIGN_BITWISE_LSHIFT, $1, $3);}
  | expr T_ASSIGN_BITWISE_RSHIFT expr
{$$ = new Node(scan, $2, Node::ASSIGN_BITWISE_RSHIFT, $1, $3);}
  | expr T_ASSIGN_POW expr
{$$ = new Node(scan, $2, Node::ASSIGN_POW, $1, $3);}
  | expr T_ASSIGN_MOD expr
{$$ = new Node(scan, $2, Node::ASSIGN_MOD, $1, $3);}
  | expr T_ASSIGN_ADD expr
{$$ = new Node(scan, $2, Node::ASSIGN_ADD, $1, $3);}
  | expr T_ASSIGN_SUB expr
{$$ = new Node(scan, $2, Node::ASSIGN_SUB, $1, $3);}
  | expr T_ASSIGN_MUL expr
{$$ = new Node(scan, $2, Node::ASSIGN_MUL, $1, $3);}
  | expr T_ASSIGN_DIV expr
{$$ = new Node(scan, $2, Node::ASSIGN_DIV, $1, $3);}
  | T_INCREMENT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::INCREMENT, $2);}
  | T_DECREMENT expr %prec P_UNARY_L
{$$ = new Node(scan, $1, Node::DECREMENT, $2);}
  | expr T_INCREMENT
{$$ = new Node(scan, $2, Node::POSTFIX_INCREMENT, $1);}
  | expr T_DECREMENT
{$$ = new Node(scan, $2, Node::POSTFIX_DECREMENT, $1);}

  /* Type-conversion operators. */

  | '[' expr ']'
{$$ = new Node(scan, $1, Node::INT_CAST, $2);
 $$->extend_bounds($3);}
  | expr '.'
{$$ = new Node(scan, $2, Node::FLOAT_CAST, $1);}
  | '(' expr_named ',' expr_list ')'
{$$ = $4;
 $$->add_front($2);
 $$->type = Node::VECTOR_CONSTRUCT;
 $$->set_inner_bounds($1);
 $$->extend_bounds($1);
 $$->extend_bounds($5);}
  | expr '[' expr_list ']'
{$$ = $3;
 $$->add_front($1);
 $$->type = Node::VECTOR_INDEX;
 $$->set_inner_bounds($2);
 $$->extend_bounds($4);}
  | identifier '{' expr '}'
{$$ = new Node(scan, $1, Node::INTERFACE_CONVERSION, $3);
 $$->string_value = $1->string_value;}
  ;

string_literal
  : T_STRING_LITERAL
{$$ = $1;}
  | string_literal T_STRING_LITERAL
{$$ = $1;
 $$->string_value += $2->string_value;
 $$->extend_inner_bounds($2);}
  ;

identifier
  : T_IDENTIFIER
{$$ = $1;}
  | identifier T_SCOPE_RESOLUTION T_IDENTIFIER
{$$ = $3;
 $$->string_value = $1->string_value + "::" + $$->string_value;
 $$->extend_inner_bounds($1);}
  ;

  /* Error-handling. */
  /* TODO: error rules, and generally more permissive parsing with error-
     -detection in the static analysis. */
