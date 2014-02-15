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

int yang_error(yyscan_t scan, const char* message)
{
  ParseData* data = (ParseData*)yang_get_extra(scan);
  data->errors.emplace_back(yang::internal::format_error(
       yang_get_lineno(scan), yang_get_text(scan), message));
  return 0;
}

%}

%pure-parser
%lex-param {yyscan_t scan}
%parse-param {yyscan_t scan}
%union {
  yang::internal::Node* node;
}

  /* Tokens. */

%token T_EOF 0

%token T_GLOBAL
%token T_EXPORT
%token T_VAR
%token T_CONST
%token T_IF
%token T_ELSE
%token T_FOR
%token T_WHILE
%token T_DO
%token T_BREAK
%token T_CONTINUE
%token T_RETURN
%token T_TYPE_LITERAL

%token T_TERNARY_L
%token T_TERNARY_R
%token T_LOGICAL_NEGATION
%token T_LOGICAL_OR
%token T_LOGICAL_AND
%token T_BITWISE_NEGATION
%token T_BITWISE_OR
%token T_BITWISE_AND
%token T_BITWISE_XOR
%token T_BITWISE_LSHIFT
%token T_BITWISE_RSHIFT
%token T_POW
%token T_MOD
%token T_ADD
%token T_SUB
%token T_MUL
%token T_DIV
%token T_EQ
%token T_NE
%token T_GE
%token T_LE
%token T_GT
%token T_LT
%token T_FOLD
%token T_SCOPE_RESOLUTION
%token T_ASSIGN
%token T_ASSIGN_LOGICAL_OR
%token T_ASSIGN_LOGICAL_AND
%token T_ASSIGN_BITWISE_OR
%token T_ASSIGN_BITWISE_AND
%token T_ASSIGN_BITWISE_XOR
%token T_ASSIGN_BITWISE_LSHIFT
%token T_ASSIGN_BITWISE_RSHIFT
%token T_ASSIGN_POW
%token T_ASSIGN_MOD
%token T_ASSIGN_ADD
%token T_ASSIGN_SUB
%token T_ASSIGN_MUL
%token T_ASSIGN_DIV
%token T_INCREMENT
%token T_DECREMENT
%token <node> T_IDENTIFIER
%token <node> T_INT_LITERAL
%token <node> T_FLOAT_LITERAL
%token <node> T_TYPE_LITERAL

  /* Operator precedence. */

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
%left '.' '[' '('
  /* Precedence of '.' for member selection. */
%left T_IDENTIFIER T_SCOPE_RESOLUTION

  /* Types. */

%type <node> program
%type <node> elem_list
%type <node> elem
%type <node> opt_export
%type <node> stmt_list
%type <node> stmt
%type <node> opt_expr
%type <node> opt_identifier
%type <node> expr_named
%type <node> expr_list
%type <node> expr_functional
%type <node> expr
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
{$$ = $1; $$->add($2);}
  ;

elem
  : opt_export T_GLOBAL stmt
{if ($3->type != Node::BLOCK) {
   // Ensure a consistent scope depth for all global declarations.
   $3 = new Node(scan, Node::BLOCK, $3);
 }
 $$ = new Node(scan, Node::GLOBAL, $3);
 $$->int_value = $1->int_value;}
  | opt_export expr T_ASSIGN expr
{$$ = new Node(scan, Node::GLOBAL_ASSIGN, $2, $4);
 $$->int_value = $1->int_value;}
  | ';'
{$$ = new Node(scan, Node::EMPTY_STMT);}
  ;

opt_export
  : T_EXPORT
{$$ = new Node(scan, Node::INT_LITERAL, 1);}
  |
{$$ = new Node(scan, Node::INT_LITERAL, 0);}
  ;

stmt_list
  :
{$$ = new Node(scan, Node::ERROR);}
  | stmt_list stmt
{$$ = $1; $$->add($2);}

stmt
  : ';'
{$$ = new Node(scan, Node::EMPTY_STMT);}
  | expr ';'
{$$ = new Node(scan, Node::EXPR_STMT, $1);}
  | T_RETURN ';'
{$$ = new Node(scan, Node::RETURN_VOID_STMT);}
  | T_RETURN expr ';'
{$$ = new Node(scan, Node::RETURN_STMT, $2);}
  | T_IF '(' expr ')' stmt %prec T_IF
{$$ = new Node(scan, Node::IF_STMT, $3, $5);}
  | T_IF '(' expr ')' stmt T_ELSE stmt
{$$ = new Node(scan, Node::IF_STMT, $3, $5, $7);}
  | T_WHILE '(' expr ')' stmt
{$$ = new Node(
     scan, Node::FOR_STMT,
     new Node(scan, Node::INT_LITERAL, 1), $3,
     new Node(scan, Node::INT_LITERAL, 1));
 $$->add($5);}
  | T_FOR '(' opt_expr ';' opt_expr ';' opt_expr ')' stmt
{$$ = new Node(scan, Node::FOR_STMT, $3, $5, new Node(scan, Node::BLOCK, $7));
 $$->add($9);}
  | T_DO stmt T_WHILE '(' expr ')' ';'
{$$ = new Node(scan, Node::DO_WHILE_STMT, $2, $5);}
  | T_BREAK ';'
{$$ = new Node(scan, Node::BREAK_STMT);}
  | T_CONTINUE ';'
{$$ = new Node(scan, Node::CONTINUE_STMT);}
  | '{' stmt_list '}'
{$$ = $2; $$->type = Node::BLOCK;}
  ;

opt_expr
  : expr
{$$ = $1;}
  |
{$$ = new Node(scan, Node::INT_LITERAL, 1);}
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
   $$ = new Node(scan, Node::NAMED_EXPRESSION, $$);
   $$->string_value = $2->string_value;
 }}
  ;

expr_list
  : expr_named
{$$ = new Node(scan, Node::ERROR, $1);}
  | expr_list ',' expr_named
{$$ = $1;
 $$->add($3);}
  ;

expr_functional
  : expr '(' ')'
{$$ = new Node(scan, Node::ERROR, $1);}
  | expr '(' expr_list ')'
{$$ = $3;
 $$->add_front($1);}
  ;

expr
  /* Complicated type-expression interaction. */
  : T_TYPE_LITERAL
{$$ = $1;}
  | T_IDENTIFIER
{$$ = $1;}
  | expr T_SCOPE_RESOLUTION T_IDENTIFIER
{$$ = new Node(scan, Node::SCOPE_RESOLUTION, $1);
 $$->string_value = $3->string_value;}
  | expr_functional '{' stmt_list '}'
{$1->type = Node::TYPE_FUNCTION;
 $3->type = Node::BLOCK;
 $$ = new Node(scan, Node::FUNCTION, $1, $3);}
  | expr_functional
{$$ = $1;
 $$->type = Node::CALL;}

  /* Miscellaeneous. */
  | '(' expr ')'
{$$ = $2;}
  | expr '.' T_IDENTIFIER
{$$ = new Node(scan, Node::MEMBER_SELECTION, $1);
 $$->string_value = $3->string_value;}
  | expr T_TERNARY_L expr T_TERNARY_R expr
{$$ = new Node(scan, Node::TERNARY, $1, $3, $5);}
  | T_INT_LITERAL
{$$ = $1;}
  | T_FLOAT_LITERAL
{$$ = $1;}

  /* Binary operators. */

  | expr T_LOGICAL_OR expr
{$$ = new Node(scan, Node::LOGICAL_OR, $1, $3);}
  | expr T_LOGICAL_AND expr
{$$ = new Node(scan, Node::LOGICAL_AND, $1, $3);}
  | expr T_BITWISE_OR expr
{$$ = new Node(scan, Node::BITWISE_OR, $1, $3);}
  | expr T_BITWISE_AND expr
{$$ = new Node(scan, Node::BITWISE_AND, $1, $3);}
  | expr T_BITWISE_XOR expr
{$$ = new Node(scan, Node::BITWISE_XOR, $1, $3);}
  | expr T_BITWISE_LSHIFT expr
{$$ = new Node(scan, Node::BITWISE_LSHIFT, $1, $3);}
  | expr T_BITWISE_RSHIFT expr
{$$ = new Node(scan, Node::BITWISE_RSHIFT, $1, $3);}
  | expr T_POW expr
{$$ = new Node(scan, Node::POW, $1, $3);}
  | expr T_MOD expr
{$$ = new Node(scan, Node::MOD, $1, $3);}
  | expr T_ADD expr
{$$ = new Node(scan, Node::ADD, $1, $3);}
  | expr T_SUB expr
{$$ = new Node(scan, Node::SUB, $1, $3);}
  | expr T_MUL expr
{$$ = new Node(scan, Node::MUL, $1, $3);}
  | expr T_DIV expr
{$$ = new Node(scan, Node::DIV, $1, $3);}
  | expr T_EQ expr
{$$ = new Node(scan, Node::EQ, $1, $3);}
  | expr T_NE expr
{$$ = new Node(scan, Node::NE, $1, $3);}
  | expr T_GE expr
{$$ = new Node(scan, Node::GE, $1, $3);}
  | expr T_LE expr
{$$ = new Node(scan, Node::LE, $1, $3);}
  | expr T_GT expr
{$$ = new Node(scan, Node::GT, $1, $3);}
  | expr T_LT expr
{$$ = new Node(scan, Node::LT, $1, $3);}

  /* Fold operators. */

  | T_FOLD T_LOGICAL_OR expr
{$$ = new Node(scan, Node::FOLD_LOGICAL_OR, $3);}
  | T_FOLD T_LOGICAL_AND expr
{$$ = new Node(scan, Node::FOLD_LOGICAL_AND, $3);}
  | T_FOLD T_BITWISE_OR expr
{$$ = new Node(scan, Node::FOLD_BITWISE_OR, $3);}
  | T_FOLD T_BITWISE_AND expr
{$$ = new Node(scan, Node::FOLD_BITWISE_AND, $3);}
  | T_FOLD T_BITWISE_XOR expr
{$$ = new Node(scan, Node::FOLD_BITWISE_XOR, $3);}
  | T_FOLD T_BITWISE_LSHIFT expr
{$$ = new Node(scan, Node::FOLD_BITWISE_LSHIFT, $3);}
  | T_FOLD T_BITWISE_RSHIFT expr
{$$ = new Node(scan, Node::FOLD_BITWISE_RSHIFT, $3);}
  | T_FOLD T_POW expr
{$$ = new Node(scan, Node::FOLD_POW, $3);}
  | T_FOLD T_MOD expr
{$$ = new Node(scan, Node::FOLD_MOD, $3);}
  | T_FOLD T_ADD expr
{$$ = new Node(scan, Node::FOLD_ADD, $3);}
  | T_FOLD T_SUB expr
{$$ = new Node(scan, Node::FOLD_SUB, $3);}
  | T_FOLD T_MUL expr
{$$ = new Node(scan, Node::FOLD_MUL, $3);}
  | T_FOLD T_DIV expr
{$$ = new Node(scan, Node::FOLD_DIV, $3);}
  | T_FOLD T_EQ expr
{$$ = new Node(scan, Node::FOLD_EQ, $3);}
  | T_FOLD T_NE expr
{$$ = new Node(scan, Node::FOLD_NE, $3);}
  | T_FOLD T_GE expr
{$$ = new Node(scan, Node::FOLD_GE, $3);}
  | T_FOLD T_LE expr
{$$ = new Node(scan, Node::FOLD_LE, $3);}
  | T_FOLD T_GT expr
{$$ = new Node(scan, Node::FOLD_GT, $3);}
  | T_FOLD T_LT expr
{$$ = new Node(scan, Node::FOLD_LT, $3);}

  /* Prefix unary operators. */

  | T_LOGICAL_NEGATION expr %prec P_UNARY_L
{$$ = new Node(scan, Node::LOGICAL_NEGATION, $2);}
  | T_BITWISE_NEGATION expr %prec P_UNARY_L
{$$ = new Node(scan, Node::BITWISE_NEGATION, $2);}
  | T_SUB expr %prec P_UNARY_L
{$$ = new Node(scan, Node::ARITHMETIC_NEGATION, $2);}
  | T_ADD expr %prec P_UNARY_L
{$$ = $2;}

  /* Assignment operators. */

  | T_VAR expr T_ASSIGN expr
{$$ = new Node(scan, Node::ASSIGN_VAR, $2, $4);}
  | T_CONST expr T_ASSIGN expr
{$$ = new Node(scan, Node::ASSIGN_CONST, $2, $4);}
  | expr T_ASSIGN expr
{$$ = new Node(scan, Node::ASSIGN, $1, $3);}
  | expr T_ASSIGN_LOGICAL_OR expr
{$$ = new Node(scan, Node::ASSIGN, $1,
               new Node(scan, Node::LOGICAL_OR, $1, $3));}
  | expr T_ASSIGN_LOGICAL_AND expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::LOGICAL_AND, $1, $3));}
  | expr T_ASSIGN_BITWISE_OR expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::BITWISE_OR, $1, $3));}
  | expr T_ASSIGN_BITWISE_AND expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::BITWISE_AND, $1, $3));}
  | expr T_ASSIGN_BITWISE_XOR expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::BITWISE_XOR, $1, $3));}
  | expr T_ASSIGN_BITWISE_LSHIFT expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::BITWISE_LSHIFT, $1, $3));}
  | expr T_ASSIGN_BITWISE_RSHIFT expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::BITWISE_RSHIFT, $1, $3));}
  | expr T_ASSIGN_POW expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::POW, $1, $3));}
  | expr T_ASSIGN_MOD expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::MOD, $1, $3));}
  | expr T_ASSIGN_ADD expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::ADD, $1, $3));}
  | expr T_ASSIGN_SUB expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::SUB, $1, $3));}
  | expr T_ASSIGN_MUL expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::MUL, $1, $3));}
  | expr T_ASSIGN_DIV expr
{$$ = new Node(scan, Node::ASSIGN, $1->clone(),
               new Node(scan, Node::DIV, $1, $3));}
  | T_INCREMENT T_IDENTIFIER %prec P_UNARY_L
{$$ = new Node(
     scan, Node::ASSIGN, $2->clone(),
     new Node(scan, Node::ADD, $2, new Node(scan, Node::INT_LITERAL, 1)));
 $$->string_value = $2->string_value;}
  | T_DECREMENT T_IDENTIFIER %prec P_UNARY_L
{$$ = new Node(
     scan, Node::ASSIGN, $2->clone(),
     new Node(scan, Node::SUB, $2, new Node(scan, Node::INT_LITERAL, 1)));
 $$->string_value = $2->string_value;}

  /* Type-conversion operators. */

  | '[' expr ']'
{$$ = new Node(scan, Node::INT_CAST, $2);}
  | expr '.'
{$$ = new Node(scan, Node::FLOAT_CAST, $1);}
  | '(' expr_named ',' expr_list ')'
{$$ = $4;
 $$->add_front($2);
 $$->type = Node::VECTOR_CONSTRUCT;}
  | expr '[' expr ']'
{$$ = new Node(scan, Node::VECTOR_INDEX, $1, $3);}
  ;

  /* Error-handling. */

  /* TODO: assign more meaningful text/line/column values to Nodes. For example,
     binary operators should have the operator, or even the entire expression
     subtree, as the text. Error messages should print carets. */
  /* TODO: error rules, and generally more permissive parsing with error-
     -detection in the static analysis. */
