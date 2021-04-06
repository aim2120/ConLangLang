(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex | List | Dict | None | UserDef of string

type typ_or_def = Typ of typ | Default

type expr =
    IntLiteral of int
  | FloatLit of string
  | BoolLit of bool
  | StrLit of string
  | ReLit of string
  | LitList of expr list
  | DictLit of (expr * expr) list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Cast of string * expr
  | ChildAcc of string * expr
  | Assign of typ * string * expr
  | ReAssign of string * expr
  | TypAssign of string * (string * typ) list
  | TypDefAssign of string * (typ * string) list
  | LId of string
  | Func of func
  | FuncCall of string * expr list
  | Match of expr * matchexpr
  | IfElse of expr * exprstmt list * exprstmt list
  | While of expr * exprstmt list
  | Expr of expr
  | Null
and func = {
    id: string;
    formals: (typ * string) list;
    typ: typ;
    body: expr list;
}
and expr_or_def = Expr of expr | Default
and matchexpr =
    ValMatch of typ * (expr_or_def * exprstmt list) list
  | TypMatch of (typ_or_def * exprstmt list) list
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
  | Dict -> "dict"
  | None -> "none"
  | UserDef(u) -> u

let string_of_typ_or_def = function
    Type(t) -> string_of_typ t
  | Default -> "default"

let rec string_of_expr = function
    IntLiteral(i) -> string_of_int i
  | FloatLit(f) -> f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrList(s) -> "'" ^ s ^ "'"
  | ReLit(r) -> '"' ^ r ^ '"'
  | LitList(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictList(d) -> "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "}"
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(s, e) -> s ^ "." string_of_expr e
  | Cast(t, e) -> "(" ^ string_of_typ t ^ ")" string_of_expr e
  | VarAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  | ReAssign(v, e) -> v ^ " = " ^ string_of_expr e
  | TypAssign(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | TypDefAssign(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ (fst p) ^ " " snd p) l) ^ "}"
  | Func(f) ->
    "fun " ^ f.id ^ " (" ^
    String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.formals) ^
    "):(" ^ string_of_type f.typ ^ ") = {\n" ^
    List.map string_of_exprstmt f.body ^ "}"
  | FuncCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Match(e, me) -> "match (" ^ string_of_expr ^ ") with " ^ string_of_match me
  | IfElse(e, l1, l2) -> "if (" ^ string_of_expr e ^ ") {\n" ^
      List.map string_of_exprstmt l1 ^ "} else {\n" ^ List.map string_of_exprstmt l2 ^ "}"
  | While(e, l) -> "while (" ^ string_of_expr ^ ") {\n" ^ List.map string_of_exprstmt l ^ "}"
  | LId(v) -> v
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Expr(e) -> "(" ^ string_of_expr e ^ ")"
and string_of_expr_or_def = function
    Expr(e) -> string_of_expr e
  | Default -> "default"
and string_of_match = function
    ValMatch(t, l) -> string_of_expr t ^ " = {\n" ^
    String.concat ";\n" (List.map (fun p -> string_of_expr_or_def (fst p) ^ " {" (List.map string_of_exprstmt (snd p)) ^ "}") l)  "}"
  | TypMatch(l) -> "type = {\n" ^
    String.concat ";\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {" (List.map string_of_exprstmt (snd p)) ^ "}") l)  "}"
and string_of_exprstmt = function
    ExprStmt(e) -> string_of_expr e ^ ";\n"

let string_of_program exprstmtlist =
    List.map string_of_exprstmt exprstmtlist

