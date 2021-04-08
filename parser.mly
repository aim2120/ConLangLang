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
%token <string> FLOATLIT STRLIT RELIT ID UT UTD
%token EOF

%start program
%type <Ast.program> program

%left ELSE
%right ASSIGN
%left OR
%left AND
%left EQ
%left LT GT
%left CONCAT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right RPAREN
%right DOT

%%

program:
    exprstmtblock EOF { List.rev $1 }

exprstmtblock:
      exprstmtblock exprstmt SEMI { $2::$1 }
    | exprstmt SEMI               { [$1] }

exprstmt:
    | expr { ExprStmt($1) }
    | TYP ID ASSIGN LCURLY typlist RCURLY { TypDecl($2, $5) }
    | TYPDEF UTD ASSIGN LCURLY decllist RCURLY { TypDefDecl($2, $5) }

typlist:
      typlist COMMA UT LANGLE typ RANGLE { ($3,$5)::$1 }
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
    | LSQUARE exprlist_opt RSQUARE { ListLit($2) }
    | LCURLY exprpairlist_opt RCURLY { DictLit($2) }
    | expr AND expr { Binop($1, And, $3) }
    | expr OR expr { Binop($1, Or, $3) }
    | expr EQ expr { Binop($1, Equal, $3) }
    | expr LT expr { Binop($1, Less, $3) }
    | expr GT expr { Binop($1, Greater, $3) }
    | expr CONCAT expr { Binop($1, Concat, $3) }
    | expr PLUS expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Sub, $3) }
    | expr MOD expr { Binop($1, Mod, $3) }
    | expr TIMES expr { Binop($1, Mult, $3) }
    | expr DIVIDE expr { Binop($1, Div, $3) }
    | NOT expr { Unop(Not, $2) }
    | MINUS expr %prec NOT { Unop(Neg, $2) }
    | LPAREN typ RPAREN expr { Cast($2, $4) }
    | ID DOT expr { ChildAcc($1, $3) }
    | typ_decl ID ASSIGN expr { Assign($1, $2, $4) }
    | ID ASSIGN expr { ReAssign($1, $3) }
    | UTD ID ASSIGN LCURLY initlist RCURLY { TypDefAssign($1, $2, $5) }
    | ID { LId($1) }
    | ID LPAREN exprlist_opt RPAREN { FuncCall($1, $3) }
    | FUN COLON typ_or_none ID LPAREN formallist_opt RPAREN ASSIGN LCURLY exprstmtblock RCURLY
      { Func({id=$4; formals=$6; typ=$3; block=(List.rev $10) }) }
    | MATCH COLON typ_or_none LPAREN expr RPAREN matchlist { Match({input=$5; typ=$3; matchlist=$7;}) }
    | IF COLON typ_or_none LPAREN expr RPAREN LCURLY exprstmtblock RCURLY ELSE LCURLY exprstmtblock RCURLY { IfElse({cond=$5; typ=$3; ifblock=(List.rev $8); elseblock=(List.rev $12);}) }
    | WHILE COLON typ_or_none LPAREN expr RPAREN LCURLY exprstmtblock RCURLY { While({cond=$5; typ=$3; block=(List.rev $8);}) }
    | LPAREN expr RPAREN { Expr($2) }
    | NULL { Null }

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

formallist_opt:
      /* nothing */ { [] }
    | formallist { List.rev $1 }

formallist:
      formallist COMMA typ ID { ($3, $4)::$1 }
    | typ ID { [($1, $2)] }

matchlist:
      BYVAL LCURLY valuematchlist RCURLY { ValMatchList(List.rev $3) }
    | BYTYP LCURLY typmatchlist RCURLY { TypMatchList(List.rev $3) }

valuematchlist:
      valuematchlist expr_or_def LCURLY exprstmtblock RCURLY { ($2,(List.rev $4))::$1 }
    | expr_or_def LCURLY exprstmtblock RCURLY { [$1,(List.rev $3)] }

typmatchlist:
      typmatchlist typ_or_def LCURLY exprstmtblock RCURLY SEMI { ($2,(List.rev $4))::$1 }
    | typ_or_def LCURLY exprstmtblock RCURLY SEMI { [$1,(List.rev $3)] }

expr_or_def:
      DEFAULT { DefaultExpr }
    | expr { ExprMatch($1) }

typ_or_def:
      DEFAULT { DefaultTyp }
    | typ { TypMatch($1) }

typ_or_none:
      NONE { None }
    | typ { TypOutput($1) }

typ_decl:
      typ { PrimDecl($1) }
    | typ LANGLE typ_decl RANGLE { ListDecl($1, $3) }
    | typ LANGLE typ_decl COMMA typ_decl RANGLE { DictDecl($1, $3, $5) }

typ:
      INT { Int }
    | FLOAT { Float }
    | BOOL { Bool }
    | STRING { String }
    | REGEX { Regex }
    | LIST { List }
    | DICT { Dict }
    | UT { UserTyp($1) }

