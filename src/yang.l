/*============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================*/
%{

#include "../src/ast.h"
#include "yang.y.h"

typedef yang::internal::Node Node;
typedef yang::internal::ParseData ParseData;

void* yang_get_extra(yyscan_t);
int yang_error(yyscan_t scan, const char* message);

// Character counting.
#define YY_USER_ACTION \
    ((ParseData*)yang_get_extra(yyscanner))->character += \
        yang_get_leng(yyscanner);
#define LVAL (yang_get_lval(yyscanner)->node)
#define TEXT (yang_get_text(yyscanner))
#define TOKEN LVAL = new Node(yyscanner, Node::ERROR)

%}

%option bison-bridge
%option reentrant
%option noyywrap
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
  TOKEN;
  return T_EOF;
}
<INITIAL>{line_comment_begin} {BEGIN(line_comment);}
<INITIAL>{long_comment_begin} {BEGIN(long_comment);}
<line_comment>{newline} {BEGIN(INITIAL);}
<long_comment>{long_comment_end} {BEGIN(INITIAL);}
<line_comment,long_comment>{default} {}
{whitespace} {}

  /* Keywords. */

global {TOKEN; return T_GLOBAL;}
export {TOKEN; return T_EXPORT;}
var {TOKEN; return T_VAR;}
const {TOKEN; return T_CONST;}
if {TOKEN; return T_IF;}
else {TOKEN; return T_ELSE;}
for {TOKEN; return T_FOR;}
while {TOKEN; return T_WHILE;}
do {TOKEN; return T_DO;}
break {TOKEN; return T_BREAK;}
continue {TOKEN; return T_CONTINUE;}
return {TOKEN; return T_RETURN;}

  /* Types. */

void {
  LVAL = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
int {
  LVAL = new Node(yyscanner, Node::TYPE_INT, 1);
  return T_TYPE_LITERAL;
}
int0 {
  LVAL = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
int{vector_size} {
  LVAL = new Node(yyscanner, Node::TYPE_INT, std::stoi(3 + TEXT));
  return T_TYPE_LITERAL;
}
float {
  LVAL = new Node(yyscanner, Node::TYPE_FLOAT, 1);
  return T_TYPE_LITERAL;
}
float0 {
  LVAL = new Node(yyscanner, Node::TYPE_VOID);
  return T_TYPE_LITERAL;
}
float{vector_size} {
  LVAL = new Node(yyscanner, Node::TYPE_FLOAT, std::stoi(5 + TEXT));
  return T_TYPE_LITERAL;
}

  /* Operators. */

"?" {TOKEN; return T_TERNARY_L;}
":" {TOKEN; return T_TERNARY_R;}
"!" {TOKEN; return T_LOGICAL_NEGATION;}
"||" {TOKEN; return T_LOGICAL_OR;}
"&&" {TOKEN; return T_LOGICAL_AND;}
"~" {TOKEN; return T_BITWISE_NEGATION;}
"|" {TOKEN; return T_BITWISE_OR;}
"&" {TOKEN; return T_BITWISE_AND;}
"^" {TOKEN; return T_BITWISE_XOR;}
"<<" {TOKEN; return T_BITWISE_LSHIFT;}
">>" {TOKEN; return T_BITWISE_RSHIFT;}
"**" {TOKEN; return T_POW;}
"%" {TOKEN; return T_MOD;}
"+" {TOKEN; return T_ADD;}
"-" {TOKEN; return T_SUB;}
"*" {TOKEN; return T_MUL;}
"/" {TOKEN; return T_DIV;}
"==" {TOKEN; return T_EQ;}
"!=" {TOKEN; return T_NE;}
">=" {TOKEN; return T_GE;}
"<=" {TOKEN; return T_LE;}
">" {TOKEN; return T_GT;}
"<" {TOKEN; return T_LT;}
"$" {TOKEN; return T_FOLD;}
"::" {TOKEN; return T_SCOPE_RESOLUTION;}

  /* Assignment operators. */

"=" {TOKEN; return T_ASSIGN;}
"||=" {TOKEN; return T_ASSIGN_LOGICAL_OR;}
"&&=" {TOKEN; return T_ASSIGN_LOGICAL_AND;}
"|=" {TOKEN; return T_ASSIGN_BITWISE_OR;}
"&=" {TOKEN; return T_ASSIGN_BITWISE_AND;}
"^=" {TOKEN; return T_ASSIGN_BITWISE_XOR;}
"<<=" {TOKEN; return T_ASSIGN_BITWISE_LSHIFT;}
">>=" {TOKEN; return T_ASSIGN_BITWISE_RSHIFT;}
"**=" {TOKEN; return T_ASSIGN_POW;}
"%=" {TOKEN; return T_ASSIGN_MOD;}
"+=" {TOKEN; return T_ASSIGN_ADD;}
"-=" {TOKEN; return T_ASSIGN_SUB;}
"*=" {TOKEN; return T_ASSIGN_MUL;}
"/=" {TOKEN; return T_ASSIGN_DIV;}
"++" {TOKEN; return T_INCREMENT;}
"--" {TOKEN; return T_DECREMENT;}

  /* Identifiers and literals. */

true {
  LVAL = new Node(yyscanner, Node::INT_LITERAL, 1);
  return T_INT_LITERAL;
}
false {
  LVAL = new Node(yyscanner, Node::INT_LITERAL, 0);
  return T_INT_LITERAL;
}
{identifier} {
  LVAL = new Node(yyscanner, Node::IDENTIFIER, TEXT);
  return T_IDENTIFIER;
}
{int_literal} {
  LVAL = new Node(yyscanner, Node::INT_LITERAL, std::stoi(TEXT));
  return T_INT_LITERAL;
}
{float_literal} {
  LVAL = new Node(yyscanner, Node::FLOAT_LITERAL, std::stod(TEXT));
  return T_FLOAT_LITERAL;
}

  /* Default. */

{default} {TOKEN; return yang_get_text(yyscanner)[0];}
