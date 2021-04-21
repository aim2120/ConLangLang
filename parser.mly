/* Ocamlyacc parser for CLL */

%{
open Ast
%}

%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE LANGLE RANGLE COLON SEMI COMMA
%token DOT MINUS TIMES DIVIDE MOD PLUS CONCAT
%token NOT AND OR EQ LT GT ASSIGN
%token INT BOOL FLOAT STRING REGEX LIST DICT FUN NONE NULL
%token TYP TYPDEF
%token MATCH BYVAL BYTYP DEFAULT WHILE IF ELSE
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> FLOATLIT STRLIT RELIT ID UTDID UT UTD
%token EOF

%start program
%type <Ast.program> program

%left ELSE
%right ASSIGN
%left OR
%left AND
%left EQ
%left LANGLE RANGLE
%left CONCAT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right RPAREN
%right DOT

%%

program:
    stmtblock EOF { List.rev $1 }

stmtblock:
      stmtblock stmt SEMI { $2::$1 }
    | stmt SEMI               { [$1] }

stmt:
    | expr { ExprStmt($1) }
    | TYP ID ASSIGN LCURLY utyplist RCURLY { TypDecl($2, $5) }
    | TYPDEF UTD ASSIGN LCURLY decllist RCURLY { TypDefDecl($2, $5) }

utyplist:
      utyplist COMMA UT LANGLE typ RANGLE { ($3,$5)::$1 }
    | UT LANGLE typ RANGLE { [($1,$3)] }

decllist:
      decllist typ ID SEMI { ($2,$3)::$1 }
    | typ ID SEMI { [($1,$2)] }

expr:
      INTLIT { IntLit($1) }
    | FLOATLIT { FloatLit($1) }
    | BOOLLIT { BoolLit($1) }
    | STRLIT { StrLit($1) }
    | RELIT { ReLit($1) }
    | LANGLE typ RANGLE LSQUARE exprlist_opt RSQUARE { ListLit($2, $5) }
    | LANGLE typ COMMA typ RANGLE LCURLY exprpairlist_opt RCURLY { DictLit($2, $4, $7) }
    | LANGLE formallist_opt COLON typ RANGLE LCURLY stmtblock RCURLY { FunLit({formals=$2; ftyp=$4; fblock=(List.rev $7);}) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | expr EQ expr { Binop($1, Equal, $3) }
    | expr LANGLE LANGLE expr { Binop($1, Less, $4) }
    | expr RANGLE RANGLE expr { Binop($1, Greater, $4) }
    | expr CONCAT expr { Binop($1, Concat, $3) }
    | expr PLUS expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Sub, $3) }
    | expr MOD expr { Binop($1, Mod, $3) }
    | expr TIMES expr { Binop($1, Mult, $3) }
    | expr DIVIDE expr { Binop($1, Div, $3) }
    | NOT expr { Unop(Not, $2) }
    | MINUS expr %prec NOT { Unop(Neg, $2) }
    | LPAREN typlist RPAREN expr { Cast(List.rev $2, $4) }
    | expr DOT ID { ChildAcc($1, $3) }
    | ID ASSIGN expr { Assign($1, $3) }
    | UTD UTDID ASSIGN LCURLY initlist RCURLY { TypDefAssign($1, $2, $5) }
    | ID { Id($1) }
    | UTDID { UTDId($1) }
    | ID LPAREN exprlist_opt RPAREN { FunCall($1, $3) }
    | MATCH COLON typ LPAREN expr RPAREN matchlist { Match({minput=$5; mtyp=$3; matchlist=$7;}) }
    | IF COLON typ LPAREN expr RPAREN LCURLY stmtblock RCURLY ELSE LCURLY stmtblock RCURLY { IfElse({icond=$5; ityp=$3; ifblock=(List.rev $8); elseblock=(List.rev $12);}) }
    | WHILE COLON typ LPAREN expr RPAREN LCURLY stmtblock RCURLY { While({wcond=$5; wtyp=$3; wblock=(List.rev $8);}) }
    | LPAREN expr RPAREN { Expr($2) }
    | NULL { NullExpr }

/*

    | FUN LANGLE typ RANGLE ID LPAREN formallist_opt RPAREN ASSIGN LCURLY stmtblock RCURLY
      { Func({id=$5; formals=$7; typ=$3; block=(List.rev $11) }) }
*/

typlist:
    typlist COMMA typ { $3::$1 }
    | typ { [$1] }

formallist_opt:
      /* nothing */ { [] }
    | formallist { List.rev $1 }

formallist:
      formallist COMMA typ ID { ($3, $4)::$1 }
    | typ ID { [($1, $2)] }

exprlist_opt:
    /* nothing */ { [] }
    | exprlist { List.rev $1 }

exprlist:
      exprlist COMMA expr { $3::$1 }
    | expr { [$1] }

exprpairlist_opt:
      /* nothing */ { [] }
    | exprpairlist { List.rev $1 }

exprpairlist:
      exprpairlist COMMA expr COLON expr { ($3,$5)::$1 }
    | expr COLON expr { [($1,$3)] }

initlist:
      initlist ID ASSIGN expr SEMI { ($2,$4)::$1 }
    | ID ASSIGN expr SEMI { [($1, $3)] }

matchlist:
      BYVAL LCURLY valuematchlist RCURLY { ValMatchList(List.rev $3) }
    | BYTYP LCURLY typmatchlist RCURLY { TypMatchList(List.rev $3) }

valuematchlist:
      valuematchlist expr_or_def LCURLY stmtblock RCURLY { ($2,(List.rev $4))::$1 }
    | expr_or_def LCURLY stmtblock RCURLY { [$1,(List.rev $3)] }

typmatchlist:
      typmatchlist typ_or_def LCURLY stmtblock RCURLY { ($2,(List.rev $4))::$1 }
    | typ_or_def LCURLY stmtblock RCURLY { [$1,(List.rev $3)] }

expr_or_def:
      DEFAULT { DefaultExpr }
    | expr { ExprMatch($1) }

typ_or_def:
      DEFAULT { DefaultTyp }
    | typ { TypMatch($1) }

typ:
      INT { Int }
    | FLOAT { Float }
    | BOOL { Bool }
    | STRING { String }
    | REGEX { Regex }
    | LIST LANGLE typ RANGLE { List($3) }
    | DICT LANGLE typ COMMA typ RANGLE { Dict($3, $5) }
    | FUN LANGLE formallist_opt COLON typ RANGLE { Fun($3, $5) }
    | UT { UserTyp($1) }
    | UTD { UserTypDef($1) }
    | NONE { Null }

