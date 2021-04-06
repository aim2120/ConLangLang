/* Ocamlyacc parser for CLL */

%{
open Ast %}

/*
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "{#"     { comment lexbuf }           (* Comments *)
| "##"     { onelinecomment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '<'      { LANGLE }
| '>'      { RANGLE }
| ':'      { COLON }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '.'      { DOT }
| "=="     { EQ }
| "<<"     { LT }
| ">>"     { GT }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "regex"  { REGEX }
| "list"   { LIST }
| "dict"   { DICT }
| "fun"    { FUN }
| "none"   { NONE }
| "type"   { TYPE }
| "typedef" { TYPEDEF }
| "true"   { BOOLLIT(true)  }
| "false"  { BOOLLIT(false) }
| "match"  { MATCH }
| "with"   { WITH }
| "default"{ DEFAULT }
| "while"  { WHILE }
| "if"     { IF }
| "else"   { ELSE }
| digits as lxm { INTLIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOATLIT(lxm) }
| lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { LID(lxm) }
| uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { UID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


 */

%token LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE COLON SEMI COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN DOT
%token EQ LT GT AND OR NOT
%token INT BOOL FLOAT STRING REGEX LIST DICT FUN NONE
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> FLOATLIT STRLIT RELIT LID UID
%token EOF

%start program
%type <Ast.program> program

%left ELSE
%right ASSIGN
%left OR
%left AND
%left EQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%right CAST
%right DOT

%%

program:
    exprstmts EOF { List.rev $1 }

exprstmts:
      exprstmts expr { $2::$1 }
    | expr         { [$1] }

expr:
      INTLIT { IntLit($1) }
    | FLOATLIT { FloatLit($1) }
    | BOOLLIT { BoolLit($1) }
    | STRLIT { StrLit($1) }
    | RELIST { ReLit{$1} }
    | LSQUARE exprlist_opt RSQUARE { LitList($1) }
    | LCURLY exprpairlist_opt RCURLY { DictLit(1) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | expr EQ expr { Binop($1, Equal, $3) }
    | expr LT expr { Binop($1, Less, $3) }
    | expr GT expr { Binop($1, Greater, $3) }
    | expr PLUS expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Sub, $3) }
    | expr MOD expr { Binop($1, Mod, $3) }
    | expr TIMES expr { Binop($1, Mult, $3) }
    | expr DIVIDE expr { Binop($1, Div, $3) }
    | NOT expr { Unop(Not, $2) }
    | MINUS expr %prec NOT { Unop(Neg $2) }
    | CAST expr { Cast($1, $2) } 
    | LID DOT expr { ChildAcc($1, $2) }
    | typ LID ASSIGN expr { Assign($1, $2, $4) }
    | LID ASSIGN expr { ReAssign($1, $3) }
    | TYPE LID ASSIGN LCURLY typlist RCURLY { TypAssign($2, $5) }
    | TYPEDEF LID ASSIGN LCURLY deccllist RCURLY { TypDefAssign($2, $5) }
    | LID { LId($1) }
    | FUN LID LPAREN formallist RPAREN COLON LPAREN typ_or_none RPAREN ASSIGN LCURLY exprstmts RCURLY
      { Func({id = $2; formals = $4; typ = $8; body = $12}) }
    | LID LPAREN exprlist_opt RPAREN { FuncCall($1, $3) }
    | MATCH LPAREN expr RPAREN WITH matchexpr { Match($3, $6) }
    | IF LPAREN expr RPAREN LCURLY exprstmts RCURLY ELSE LCURLY exprstmts RCURLY { IfElse($3, $6, $9) }
    | WHILE LPAREN expr RPAREN LCURLY exprstmts RCURLY { While($3, $6) }
    | LPAREN expr RPAREN { Expr($2) }
    | NULL { Null }

exprlist_opt: 
    /* nothing */ { [] }
    | exprlist { List.rev $1 }

exprlist:
      exprlist COMMA expr { $2::$1 }
    | expr { [$1] }

exprpairlist_opt:
      /* nothing */ { [] }
    | exprpairlist { List.rev $1 }

exprpairlist:
      exprpairlist COMMA expr COLON expr { ($3,$5)::$1 }
    | expr COLON expr { [($1,$3)] }

typlist:
      typlist COMMA UID LANGLE typ RANGLE { ($3,$5)::$1 }
    | UID LANGLE typ RANGLE { [($1,$3)] }

decllist:
      decllist SEMI typ LID SEMI { ($3,$4)::$1 }
    | typ LID SEMI { [($1,$2)] }

formallist_opt:
      /* nothing */ { [] }
    | formallist { List.rev $1 }

formallist:
      formallist COMMA expr { $3::$1 }
    | expr { [$1] }

matchexpr:
      valuematch { $1 }
    | typmatch { $1 }

valuematch:
    typ LCURLY valuematchlist RCURLY { ValMatch($1, List.rev $3) }

typmatch:
      TYPE LCURLY typmatchlist RCURLY { TypMatch(List.rev $3) }
    
valuematchlist:
    valuematchlist SEMI expr_or_def LCURLY exprstmts RCURLY { ($3,(List.rev $5))::$1 }
    | expr_or_def LCURLY exprstmts RCURLY { [$1,(List.rev $3)] }

typmatchlist:
    typmatchlist SEMI typ_or_def LCURLY exprstmts RCURLY { ($3,(List.rev $5))::$1 }
    | typ_or_def LCURLY exprstmts RCURLY { [$1,(List.rev $3)] }
/*
    ValMatch of typ * (expr_or_def * exprstmt) list
  | TypMatch of typ_or_def * exprstmt list
/* LEFT OFF HERE */
*/

typ_or_none:
    NONE { None }
    | typ { $1 } 


typ:
      INT { Int }
    | FLOAT { Float }
    | BOOL { Bool }
    | STRING { String }
    | REGEX { Regex }
    | LIST { List }
    | DICT { Dict }
    | UID { UserDef($1) }

