(* Ocamllex scanner for CLL *)

{
open Parser

let line_num: int ref = ref 1
}

let digit = ['0' - '9']
let digits = digit+
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| '\n'     { line_num := !line_num + 1; token lexbuf }
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
| '\'' [^ '\'']* '\'' as lxm { let len = String.length lxm in STRLIT(String.sub lxm 1 (len - 2)) }
| '"' [^ '"']* '"' as lxm { let len = String.length lxm in RELIT(String.sub lxm 1 (len - 2)) }
| lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '$' lowercase ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { UTDID(lxm) }
| uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { UT(lxm) }
| '$' uppercase ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { UTD(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
 '\n'  { line_num := !line_num + 1; comment lexbuf }
| "#}" { token lexbuf }
| _    { comment lexbuf }

and onelinecomment = parse
  '\n' { line_num := !line_num + 1; token lexbuf }
| _    { onelinecomment lexbuf }
