%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;          /* double value */
  std::string          *s;          /* symbol name or string literal */
  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  til::block_node      *block;
};

%token tAND tOR tGE tLE tEQ tNE tSIZEOF
%token tREAD tPRINT tPRINTLN
%token tPUBLIC tPRIVATE tFORWARD tEXTERNAL tVAR
%token tTYPE_INT tTYPE_DOUBLE tTYPE_STRING tTYPE_VOID tTYPE_POINTER tADDR
%token tIF tELSE
%token tWHILE
%token tPROGRAM
%token tSTOP tNEXT tRETURN

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING tRECURSION
%token <expression> tNULL

%token tBEGIN tEND tFUNCTION tSET tINDEX
%token tBLOCK tOBJECTS

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-' '~'
%left '*' '/' '%'
%nonassoc tUNARY

%type<node> program declaration instruction return next stop conditional
%type<expression> expr integer double opt_initializer function call
%type<lvalue> lval
%type<sequence> file declarations expressions instructions
%type<block> block
%type<type> type functype types

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file :              program    { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     | declarations            { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     | declarations program    { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     |                         { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     ;

program : '(' tPROGRAM declarations instructions ')' { $$ = new til::program_node(LINE, $3, $4); }
        | '(' tPROGRAM declarations              ')' { $$ = new til::program_node(LINE, $3, nullptr); }
        | '(' tPROGRAM              instructions ')' { $$ = new til::program_node(LINE, nullptr, $3); }
        | '(' tPROGRAM                           ')' { $$ = new til::program_node(LINE, nullptr, nullptr); }
        ;

declarations : declaration                   { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration      { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;


opt_initializer  : /* empty */           { $$ = nullptr; }
                 | expr                  { $$ = $1; }
                 ;

declaration    : '(' tPUBLIC type tIDENTIFIER  opt_initializer ')'   { $$ = new til::variable_declaration_node(LINE, tPUBLIC, *$4, $5, $3); }
               | '('         type tIDENTIFIER  opt_initializer ')'   { $$ = new til::variable_declaration_node(LINE, tPRIVATE, *$3, $4, $2); }
               | '(' tPUBLIC tVAR tIDENTIFIER  expr ')'              { $$ = new til::variable_declaration_node(LINE, tPUBLIC, *$4, $5, nullptr); }
               | '('         tVAR tIDENTIFIER  expr ')'              { $$ = new til::variable_declaration_node(LINE, tPRIVATE, *$3, $4, nullptr); }
               | '(' tPUBLIC      tIDENTIFIER  expr ')'              { $$ = new til::variable_declaration_node(LINE, tPUBLIC, *$3, $4, nullptr); }
               | '(' tFORWARD      tIDENTIFIER expr            ')'   { $$ = new til::variable_declaration_node(LINE, tFORWARD, *$3, $4, nullptr); }
               | '(' tFORWARD type tIDENTIFIER opt_initializer ')'   { $$ = new til::variable_declaration_node(LINE, tFORWARD, *$4, $5, $3); }
               | '(' tEXTERNAL      tIDENTIFIER expr           ')'   { $$ = new til::variable_declaration_node(LINE, tEXTERNAL, *$3, $4, nullptr); }
               | '(' tEXTERNAL type tIDENTIFIER opt_initializer ')'  { $$ = new til::variable_declaration_node(LINE, tEXTERNAL, *$4, $5, $3); }
               ;

call : '(' lval expressions ')'       { $$ = new til::call_node(LINE, new cdk::rvalue_node(LINE, $2), $3); }
     | '(' lval             ')'       { $$ = new til::call_node(LINE, new cdk::rvalue_node(LINE, $2)); }
     | '(' tRECURSION expressions ')' { $$ = new til::call_node(LINE, new cdk::string_node(LINE, $2), $3); }
     | '(' tRECURSION             ')' { $$ = new til::call_node(LINE, new cdk::string_node(LINE, $2)); }
     | '(' function expressions ')'   { $$ = new til::call_node(LINE, $2, $3); }
     | '(' function             ')'   { $$ = new til::call_node(LINE, $2); }
     ;

function : '(' tFUNCTION '(' type ')' declarations instructions ')'              { $$ = new til::function_definition_node(LINE, nullptr, $6, $7); }
         | '(' tFUNCTION '(' type ')' declarations              ')'              { $$ = new til::function_definition_node(LINE, nullptr, $6, nullptr); }
         | '(' tFUNCTION '(' type ')'              instructions ')'              { $$ = new til::function_definition_node(LINE, nullptr, nullptr, $6); }
         | '(' tFUNCTION '(' type ')'                           ')'              { $$ = new til::function_definition_node(LINE, nullptr, nullptr, nullptr); }
         | '(' tFUNCTION '(' type declarations ')' declarations instructions ')' { $$ = new til::function_definition_node(LINE, $5, $7, $8); }
         | '(' tFUNCTION '(' type declarations ')' declarations              ')' { $$ = new til::function_definition_node(LINE, $5, $7, nullptr); }
         | '(' tFUNCTION '(' type declarations ')'              instructions ')' { $$ = new til::function_definition_node(LINE, $5, nullptr, $7); }
         | '(' tFUNCTION '(' type declarations ')'                           ')' { $$ = new til::function_definition_node(LINE, $5, nullptr, nullptr); }
         ;

type : tTYPE_INT                    { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tTYPE_DOUBLE                 { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tTYPE_STRING                 { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | type tTYPE_POINTER           { $$ = cdk::reference_type::create(4, $1); }
     | tTYPE_VOID                        { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | functype                     { $$ = $1; }
     ;

functype : '(' type ')'               { $$ = $2; }
         | '(' type '(' types ')' ')' { $$ = $2; }
         ;

types : type                      { $$ = $1; }
      | types type                { $$ = $2; }
      ;

block : '(' tBLOCK  declarations instructions ')'  { $$ = new til::block_node(LINE, $3, $4); }
      | '(' tBLOCK  declarations              ')'  { $$ = new til::block_node(LINE, $3, nullptr); }
      | '(' tBLOCK               instructions ')'  { $$ = new til::block_node(LINE, nullptr, $3); }
      | '(' tBLOCK                            ')'  { $$ = new til::block_node(LINE, nullptr, nullptr); }
      ;

instruction : expr                            { $$ = new til::evaluation_node(LINE, $1); }
            | '(' tPRINT    expressions ')'   { $$ = new til::print_node(LINE, $3, false); }
            | '(' tPRINTLN  expressions ')'   { $$ = new til::print_node(LINE, $3, true); }
            | stop                            { $$ = $1; }
            | next                            { $$ = $1; }
            | return                          { $$ = $1; }
            | block                           { $$ = $1; }
            | conditional                     { $$ = $1; }
            | '(' tWHILE expr instruction ')' { $$ = new til::while_node(LINE, $3, $4); }
            ;

conditional : '(' tIF expr instruction ')' %prec tIFX  { $$ = new til::if_node(LINE, $3, $4); }
            | '(' tIF expr instruction instruction ')' { $$ = new til::if_else_node(LINE, $3, $4, $5); }
            ;

instructions : instruction                  { $$ = new cdk::sequence_node(LINE, $1); }
             | instructions instruction     { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

return : '(' tRETURN expr ')'        { $$ = new til::return_node(LINE, $3); }
       | '(' tRETURN      ')'        { $$ = new til::return_node(LINE); }
       ;

next : '(' tNEXT tINTEGER ')'       { $$ = new til::next_node(LINE, $3); }
     | '(' tNEXT          ')'       { $$ = new til::next_node(LINE); }
     ;

stop : '(' tSTOP tINTEGER ')'       { $$ = new til::stop_node(LINE, $3); }
     | '(' tSTOP          ')'       { $$ = new til::stop_node(LINE); }
     ;

expressions : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
            | expressions expr          { $$ = new cdk::sequence_node(LINE, $2, $1); }
            ;

expr : integer                  { $$ = $1; }
     | double                   { $$ = $1; }
     | tSTRING                  { $$ = new cdk::string_node(LINE, $1); }
     | tNULL                    { $$ = new til::null_node(LINE); }
     /* FUNCTION */
     | call                     { $$ = $1; }
     | function                 { $$ = $1; }
     /* LEFT VALUES */
     | lval                     { $$ = new cdk::rvalue_node(LINE, $1); }
     /* ASSIGNMENTS */
     | '(' tSET lval expr ')'   { $$ = new cdk::assignment_node(LINE, $3, $4); }
     /* UNARY EXPRESSION */
     | '(' '-' expr %prec tUNARY ')'   { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr %prec tUNARY ')'   { $$ = new cdk::unary_plus_node(LINE, $3); }
     /* ARITHMETIC EXPRESSIONS */
     | '(' '+' expr expr ')'    { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'    { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr expr ')'    { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'    { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'    { $$ = new cdk::mod_node(LINE, $3, $4); }
     /* LOGICAL EXPRESSIONS */
     | '(' '<' expr expr ')'    { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'    { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'    { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'    { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'    { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'    { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' tAND expr expr ')'   { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'    { $$ = new cdk::or_node(LINE, $3, $4); }
     | '(' '~' expr ')'         { $$ = new cdk::not_node(LINE, $3); }
     /* READ */
     | '(' tREAD ')'            { $$ = new til::read_node(LINE); }
     /* SIZEOF */
     | '(' tSIZEOF expr ')'     { $$ = new til::size_of_node(LINE, $3); }
     | '(' tOBJECTS expr ')'    { $$ = new til::alloc_node(LINE, $3); }
     | '(' tADDR lval ')'       { $$ = new til::address_node(LINE, $3); }
     ;

lval : tIDENTIFIER                  { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')'     { $$ = new til::index_node(LINE, $3, $4); }
     ;

integer   : tINTEGER                      { $$ = new cdk::integer_node(LINE, $1); };
double    : tDOUBLE                       { $$ = new cdk::double_node(LINE, $1); };

%%
