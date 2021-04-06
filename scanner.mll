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
| '(' ['a'-'z' 'A'-'Z' '0'-'9' '_']* ')' as lxm { CAST(lxm) }
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
| "<<"     { LT }
| ">>"     { GT }
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
| "'" _* "'" as lxm { STRLIT(lxm) }
| '"' _* '"' as lxm { RELIT(lxm) }
| lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { LID(lxm) }
| uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { UID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#}" { token lexbuf }
| _    { comment lexbuf }

and onelinecomment = parse
  '\n' { token lexbuf }
| _    { onelinecomment lexbuf }
