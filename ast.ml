(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex | List | Dict | UserDef of string

type typ_or_def = TypMatch of typ | DefaultTyp

type typ_or_none = TypOutput of typ | None

type expr =
    IntLit of int
  | FloatLit of string
  | BoolLit of bool
  | StrLit of string
  | ReLit of string
  | ListLit of expr list
  | DictLit of (expr * expr) list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Cast of typ * expr
  | ChildAcc of string * expr
  | Assign of typ * string * expr
  | ReAssign of string * expr
  | TypAssign of string * (string * typ) list
  | TypDefAssign of string * (typ * string) list
  | LId of string
  | FuncCall of string * expr list
  | Func of func
  | Match of mtch
  | IfElse of ifelse
  | While of whle
  | Expr of expr
  | Null
and func = {
    id: string;
    formals: (typ * string) list;
    typ: typ_or_none;
    block: exprstmt list;
}
and mtch = {
    input: expr;
    typ: typ_or_none;
    matchlist: matchlist;
}
and matchlist =
    ValMatchList of (expr_or_def * exprstmt list) list
  | TypMatchList of (typ_or_def * exprstmt list) list
and expr_or_def = ExprMatch of expr | DefaultExpr
and ifelse = {
    cond: expr;
    typ: typ_or_none;
    ifblock: exprstmt list;
    elseblock: exprstmt list;
}
and whle = {
    cond: expr;
    typ: typ_or_none;
    block: exprstmt list;
}
and exprstmt = ExprStmt of expr

type program = exprstmt list

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

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Regex -> "regex"
  | List -> "list"
  | Dict -> "dict"
  | UserDef(u) -> u

let string_of_typ_or_def = function
    TypMatch(t) -> string_of_typ t
  | DefaultTyp -> "default"

let string_of_typ_or_none = function
    TypOutput(t) -> string_of_typ t
  | None -> "none"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> "'" ^ s ^ "'"
  | ReLit(r) -> "\"" ^ r ^ "\""
  | ListLit(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictLit(d) -> "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "}"
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(s, e) -> s ^ "." ^ string_of_expr e
  | Cast(t, e) -> "(" ^ string_of_typ t ^ ")" ^ string_of_expr e
  | Assign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  | ReAssign(v, e) -> v ^ " = " ^ string_of_expr e
  | TypAssign(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | TypDefAssign(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "}"
  | FuncCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Func(f) -> "fun:" ^ string_of_typ_or_none f.typ ^ " " ^ f.id ^ " (" ^
    String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.formals) ^
    ") = {\n" ^ string_of_exprstmtblock f.block ^ "}"
  | Match(m) -> "match:" ^ string_of_typ_or_none m.typ ^ " (" ^ string_of_expr m.input ^ ")" ^ string_of_matchlist m.matchlist
  | IfElse(i) -> "if:" ^ string_of_typ_or_none i.typ ^ " (" ^ string_of_expr i.cond ^ ") {\n" ^
      string_of_exprstmtblock i.ifblock ^ "} else {\n" ^ string_of_exprstmtblock i.elseblock ^ "}"
  | While(w) -> "while:" ^ string_of_typ_or_none w.typ ^ " (" ^ string_of_expr w.cond ^ ") {\n" ^ string_of_exprstmtblock w.block ^ "}"
  | LId(v) -> v
  | Expr(e) -> "(" ^ string_of_expr e ^ ")"
and string_of_expr_or_def = function
    ExprMatch(e) -> string_of_expr e
  | DefaultExpr -> "default"
and string_of_matchlist = function
    ValMatchList(l) -> "byvalue  {\n" ^
    String.concat ";\n" (List.map (fun p -> string_of_expr_or_def (fst p) ^ " {" ^ string_of_exprstmtblock (snd p) ^ "}") l) ^ "}"
  | TypMatchList(l) -> "bytype {\n" ^
    String.concat ";\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {" ^ string_of_exprstmtblock (snd p) ^ "}") l) ^ "}"
and string_of_exprstmt = function
    ExprStmt(e) -> string_of_expr e
and string_of_exprstmtblock l = String.concat "" (List.map (fun e -> string_of_exprstmt e ^ ";\n") l)

let string_of_program exprstmtblock =
    string_of_exprstmtblock exprstmtblock

