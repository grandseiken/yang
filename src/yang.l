/*============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================*/
%{

#include "../src/ast.h"
#include "yang.y.h"

typedef yang::internal::Node Node;
int yang_error(yyscan_t scan, const char* message);

%}

%option bison-bridge
%option reentrant
%option noyywrap
%option yylineno
%x line_comment
%x long_comment

  /* Regular expressions. */

newline \n|\r|\r\n
whitespace [ ]+|{newline}
whitespace_newline [ ]+{newline}
tab \t
line_comment_begin \/\/
long_comment_begin \/\*
long_comment_end \*\/
default .|\n|\r

alpha [A-Za-z_]
digit [0-9]
identifier {alpha}({alpha}|{digit})*
int_literal {digit}+
float_literal ({digit}+)?\.{digit}+
vector_size [1-9]{digit}*

%%

  /* Comments and whitespace. */

<*>{tab} {
  yang_error(yyscanner, "tab character");
}
<*>{whitespace_newline} {
  yang_error(yyscanner, "end-of-line whitespace");
}
<long_comment><<EOF>> {
  yang_error(yyscanner, "end-of-input inside comment");
  BEGIN(INITIAL);
  return T_EOF;
}
<INITIAL>{line_comment_begin} {BEGIN(line_comment);}
<INITIAL>{long_comment_begin} {BEGIN(long_comment);}
<line_comment>{newline} {BEGIN(INITIAL);}
<long_comment>{long_comment_end} {BEGIN(INITIAL);}
<line_comment,long_comment>{default} {}
{whitespace} {}

  /* Keywords. */

global {return T_GLOBAL;}
export {return T_EXPORT;}
var {return T_VAR;}
const {return T_CONST;}
if {return T_IF;}
else {return T_ELSE;}
for {return T_FOR;}
while {return T_WHILE;}
do {return T_DO;}
break {return T_BREAK;}
continue {return T_CONTINUE;}
return {return T_RETURN;}

  /* Types. */

void {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
int {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::TYPE_INT, 1);
  return T_TYPE_LITERAL;
}
int0 {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
int{vector_size} {
  yang_get_lval(yyscanner)->node =
      new Node(yyscanner, Node::TYPE_INT,
               std::stoi(3 + yang_get_text(yyscanner)));
  return T_TYPE_LITERAL;
}
float {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::TYPE_FLOAT, 1);
  return T_TYPE_LITERAL;
}
float0 {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
float{vector_size} {
  yang_get_lval(yyscanner)->node =
      new Node(yyscanner, Node::TYPE_FLOAT,
               std::stoi(5 + yang_get_text(yyscanner)));
  return T_TYPE_LITERAL;
}

  /* Operators. */

"?" {return T_TERNARY_L;}
":" {return T_TERNARY_R;}
"!" {return T_LOGICAL_NEGATION;}
"||" {return T_LOGICAL_OR;}
"&&" {return T_LOGICAL_AND;}
"~" {return T_BITWISE_NEGATION;}
"|" {return T_BITWISE_OR;}
"&" {return T_BITWISE_AND;}
"^" {return T_BITWISE_XOR;}
"<<" {return T_BITWISE_LSHIFT;}
">>" {return T_BITWISE_RSHIFT;}
"**" {return T_POW;}
"%" {return T_MOD;}
"+" {return T_ADD;}
"-" {return T_SUB;}
"*" {return T_MUL;}
"/" {return T_DIV;}
"==" {return T_EQ;}
"!=" {return T_NE;}
">=" {return T_GE;}
"<=" {return T_LE;}
">" {return T_GT;}
"<" {return T_LT;}
"$" {return T_FOLD;}
"::" {return T_SCOPE_RESOLUTION;}

  /* Assignment operators. */

"=" {return T_ASSIGN;}
"||=" {return T_ASSIGN_LOGICAL_OR;}
"&&=" {return T_ASSIGN_LOGICAL_AND;}
"|=" {return T_ASSIGN_BITWISE_OR;}
"&=" {return T_ASSIGN_BITWISE_AND;}
"^=" {return T_ASSIGN_BITWISE_XOR;}
"<<=" {return T_ASSIGN_BITWISE_LSHIFT;}
">>=" {return T_ASSIGN_BITWISE_RSHIFT;}
"**=" {return T_ASSIGN_POW;}
"%=" {return T_ASSIGN_MOD;}
"+=" {return T_ASSIGN_ADD;}
"-=" {return T_ASSIGN_SUB;}
"*=" {return T_ASSIGN_MUL;}
"/=" {return T_ASSIGN_DIV;}
"++" {return T_INCREMENT;}
"--" {return T_DECREMENT;}

  /* Identifiers and literals. */

true {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::INT_LITERAL, 1);
  return T_INT_LITERAL;
}
false {
  yang_get_lval(yyscanner)->node = new Node(yyscanner, Node::INT_LITERAL, 0);
  return T_INT_LITERAL;
}
{identifier} {
  yang_get_lval(yyscanner)->node =
      new Node(yyscanner, Node::IDENTIFIER, yang_get_text(yyscanner));
  return T_IDENTIFIER;
}
{int_literal} {
  yang_get_lval(yyscanner)->node =
      new Node(yyscanner, Node::INT_LITERAL, std::stoi(yang_get_text(yyscanner)));
  return T_INT_LITERAL;
}
{float_literal} {
  yang_get_lval(yyscanner)->node =
      new Node(yyscanner, Node::FLOAT_LITERAL, std::stod(yang_get_text(yyscanner)));
  return T_FLOAT_LITERAL;
}

  /* Default. */

{default} {return yang_get_text(yyscanner)[0];}
