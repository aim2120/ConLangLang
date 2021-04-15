(* Ocamllex scanner for CLL *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "{#"     { comment lexbuf }           (* Comments *)
| "##"     { onelinecomment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LCURLY }
| '}'      { RCURLY }
| '['      { LSQUARE }
| ']'      { RSQUARE }
| '<'      { LANGLE }
| '>'      { RANGLE }
| ':'      { COLON }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '+'      { PLUS }
| '^'      { CONCAT }
| "!"      { NOT }
| "&&"     { AND }
| "||"     { OR }
| "=="     { EQ }
| '='      { ASSIGN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "regex"  { REGEX }
| "list"   { LIST }
| "dict"   { DICT }
| "fun"    { FUN }
| "none"   { NONE }
| "null"   { NULL }
| "type"   { TYP }
| "typedef" { TYPDEF }
| "match"  { MATCH }
| "byvalue"{ BYVAL }
| "bytype" { BYTYP }
| "default"{ DEFAULT }
| "while"  { WHILE }
| "if"     { IF }
| "else"   { ELSE }
| digits as lxm { INTLIT(int_of_string lxm) }
| "true"   { BOOLLIT(true)  }
| "false"  { BOOLLIT(false) }
| digits '.'  digit* as lxm { FLOATLIT(lxm) }
| '\'' [^ '\'']* '\'' as lxm { STRLIT(lxm) }
| '"' [^ '"']* '"' as lxm { RELIT(lxm) }
| lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '$' lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { UTDID(lxm) }
| uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { UT(lxm) }
| '$' uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { UTD(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#}" { token lexbuf }
| _    { comment lexbuf }

and onelinecomment = parse
  '\n' { token lexbuf }
| _    { onelinecomment lexbuf }
