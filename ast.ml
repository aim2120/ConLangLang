(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex
  | List of typ
  | Dict of typ * typ
  | Fun of (typ * string) list * typ
  | UserTyp of string
  | UserTypDef of string
  | None

type typ_or_def = TypMatch of typ | DefaultTyp

type expr =
    IntLit of int
  | FloatLit of string
  | BoolLit of bool
  | StrLit of string
  | ReLit of string
  | ListLit of typ * expr list
  | DictLit of typ * typ * (expr * expr) list
  | FunLit of funlit
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Cast of typ list * expr
  | ChildAcc of expr * string
  | Assign of string * expr
  | TypDefAssign of string * string * (string * expr) list
  | Id of string
  | UTDId of string
  | FunCall of string * expr list
  | Match of mtch
  | IfElse of ifelse
  | While of whle
  | Expr of expr
  | Null
and funlit = {
    formals: (typ * string) list;
    typ: typ;
    block: stmt list;
}
and mtch = {
    input: expr;
    typ: typ;
    matchlist: matchlist;
}
and matchlist =
    ValMatchList of (expr_or_def * stmt list) list
  | TypMatchList of (typ_or_def * stmt list) list
and expr_or_def = ExprMatch of expr | DefaultExpr
and ifelse = {
    cond: expr;
    typ: typ;
    ifblock: stmt list;
    elseblock: stmt list;
}
and whle = {
    cond: expr;
    typ: typ;
    block: stmt list;
}
and stmt =
    ExprStmt of expr
  | TypDecl of string * (string * typ) list
  | TypDefDecl of string * (typ * string) list

type program = stmt list

(* Pretty-printing functions *)

let string_of_binop = function
    Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Add -> "+"
  | Sub -> "-"
  | Concat -> "^"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | Greater -> ">>"
  | Less -> "<<"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Regex -> "regex"
  | List(t) -> "list" ^ "<" ^ string_of_typ t ^ ">"
  | Dict(t1,t2) -> "dict" ^ "<" ^ string_of_typ t1 ^ "," ^ string_of_typ t2 ^ ">"
  | Fun(f,t) -> "fun" ^ "<" ^ String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f) ^ ":" ^ string_of_typ t ^ ">"
  | UserTyp(u) -> u
  | UserTypDef(u) -> u
  | None -> "none"

let string_of_typ_or_def = function
    TypMatch(t) -> string_of_typ t
  | DefaultTyp -> "default"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> "'" ^ String.sub s 1 ((String.length s) - 2)  ^ "'" (* substring removes quotes around string *)
  | ReLit(r) -> "\"" ^ String.sub r 1 ((String.length r) - 2) ^ "\"" (* substring removes quotes around string *)
  | ListLit(t, l) -> "<" ^ string_of_typ t ^ ">" ^ "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictLit(t1, t2, d) -> "<" ^ string_of_typ t1 ^ "," ^ string_of_typ t2 ^ ">" ^ "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "\n}"
  | FunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.formals) ^ ":" ^
    string_of_typ f.typ ^ ">{\n" ^ string_of_stmtblock f.block ^ "}"
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(e, s) -> string_of_expr e ^ "." ^ s
  | Cast(t, e) -> "(" ^ String.concat ", " (List.map string_of_typ t) ^ ")" ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | TypDefAssign(t, v, l) -> t ^ " " ^ v ^ " = {\n" ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_expr (snd p) ^ ";\n") l) ^ "}"
  | FunCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Match(m) -> "match:" ^ string_of_typ m.typ ^ " (" ^ string_of_expr m.input ^ ")" ^ string_of_matchlist m.matchlist
  | IfElse(i) -> "if:" ^ string_of_typ i.typ ^ " (" ^ string_of_expr i.cond ^ ") {\n" ^
      string_of_stmtblock i.ifblock ^ "} else {\n" ^ string_of_stmtblock i.elseblock ^ "}"
  | While(w) -> "while:" ^ string_of_typ w.typ ^ " (" ^ string_of_expr w.cond ^ ") {\n" ^ string_of_stmtblock w.block ^ "}"
  | Id(v) -> v
  | UTDId(v) -> v
  | Expr(e) -> "(" ^ string_of_expr e ^ ")"
and string_of_expr_or_def = function
    ExprMatch(e) -> string_of_expr e
  | DefaultExpr -> "default"
and string_of_matchlist = function
    ValMatchList(l) -> " byvalue  {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_expr_or_def (fst p) ^ " {\n" ^ string_of_stmtblock (snd p) ^ "}") l) ^ "\n}"
  | TypMatchList(l) -> " bytype {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {\n" ^ string_of_stmtblock (snd p) ^ "}") l) ^ "\n}"
and string_of_stmt = function
    ExprStmt(e) -> string_of_expr e
  | TypDecl(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | TypDefDecl(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "\n}"

and string_of_stmtblock l = String.concat "" (List.map (fun e -> string_of_stmt e ^ ";\n") l)

let string_of_program stmtblock =
    string_of_stmtblock stmtblock

