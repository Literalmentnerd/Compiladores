%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError
%}

%x X_STRING 
%x X_COMMENT
%x X_HEX_INT
%%
  yydebug=1; set_debug(1);

";".*$                 

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|"\n"      ;

"int"                    return tTYPE_INT;
"double"                 return tTYPE_DOUBLE;
"string"                 return tTYPE_STRING;
"!"                      return tTYPE_POINTER;
"void"                   return tTYPE_VOID;
"?"                      return tADDR;

"null"                 return tNULL;
"sizeof"               return tSIZEOF;

"public"               return tPUBLIC;
"forward"              return tFORWARD;
"external"             return tEXTERNAL;
"var"                  return tVAR;

"if"                   return tIF; 
"else"                 return tELSE; 

"while"                return tWHILE;
"loop"                 return tWHILE;
"next"                 return tNEXT;
"stop"                 return tSTOP;

">="                   return tGE;
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 
"&&"                   return tAND;
"||"                   return tOR;
"set"                  return tSET;

"return"               return tRETURN;
"print"                return tPRINT;
"println"              return tPRINTLN; 


"read"                 return tREAD;
"function"             return tFUNCTION;
"@"                    return tRECURSION;
"program"              return tPROGRAM;
"index"                return tINDEX;
"block"                return tBLOCK;
"objects"              return tOBJECTS;
"begin"                return tBEGIN; 
"end"                  return tEND; 

"0x"                   yy_push_state(X_HEX_INT);

<X_HEX_INT>[[:xdigit:]]+      { yylval.i = strtoul(yytext, NULL, 16); yy_pop_state(); return tINTEGER; }
<X_HEX_INT>.|\n               std::cerr << "WARNING: line " << lineno() << ": bad hexadecimal data!" << std::endl; exit(1);

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTRING;
<X_STRING>\\\"         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;
([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { yylval.d = strtod(yytext, NULL); return tDOUBLE; }

[-()<>=+*/%;{}.~]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
