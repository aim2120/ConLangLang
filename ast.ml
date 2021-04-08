(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex | List | Dict | UserDef of string

type typ_decl =
    PrimDecl of typ
  | ListDecl of typ * typ_decl
  | DictDecl of typ * typ_decl * typ_decl

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
  | Assign of typ_decl * string * expr_or_initlist
  | ReAssign of string * expr_or_initlist
  | TypAssign of string * (string * typ) list
  | TypDefDecl of string * (typ * string) list
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
and expr_or_initlist =
    AssignExpr of expr
  | InitList of (string * expr) list
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

let rec string_of_typ_decl = function
    PrimDecl(t) -> string_of_typ t
  | ListDecl(l, t) -> "list<" ^ string_of_typ_decl t ^ ">"
  | DictDecl(d, t1, t2) -> "dict<" ^ string_of_typ_decl t1 ^ "," ^ string_of_typ_decl t2 ^ ">"

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
  | StrLit(s) -> "'" ^ String.sub s 1 ((String.length s) - 2)  ^ "'" (* substring removes quotes around string *)
  | ReLit(r) -> "\"" ^ String.sub r 1 ((String.length r) - 2) ^ "\"" (* substring removes quotes around string *)
  | ListLit(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictLit(d) -> "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "\n}"
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(s, e) -> s ^ "." ^ string_of_expr e
  | Cast(t, e) -> "(" ^ string_of_typ t ^ ")" ^ string_of_expr e
  | Assign(t, v, e) -> string_of_typ_decl t ^ " " ^ v ^ " = " ^ string_of_expr_or_initlist e
  | ReAssign(v, e) -> v ^ " = " ^ string_of_expr_or_initlist e
  | TypAssign(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | TypDefDecl(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "\n}"
  | FuncCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Func(f) -> "fun:" ^ string_of_typ_or_none f.typ ^ " " ^ f.id ^ " (" ^
    String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.formals) ^
    ") = {\n" ^ string_of_exprstmtblock f.block ^ "\n}"
  | Match(m) -> "match:" ^ string_of_typ_or_none m.typ ^ " (" ^ string_of_expr m.input ^ ")" ^ string_of_matchlist m.matchlist
  | IfElse(i) -> "if:" ^ string_of_typ_or_none i.typ ^ " (" ^ string_of_expr i.cond ^ ") {\n" ^
      string_of_exprstmtblock i.ifblock ^ "} else {\n" ^ string_of_exprstmtblock i.elseblock ^ "}"
  | While(w) -> "while:" ^ string_of_typ_or_none w.typ ^ " (" ^ string_of_expr w.cond ^ ") {\n" ^ string_of_exprstmtblock w.block ^ "\n}"
  | LId(v) -> v
  | Expr(e) -> "(" ^ string_of_expr e ^ ")"
and string_of_expr_or_def = function
    ExprMatch(e) -> string_of_expr e
  | DefaultExpr -> "default"
and string_of_expr_or_initlist = function
    AssignExpr(e) -> string_of_expr e
  | InitList(l) -> String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_expr (snd p) ^ ";") l)
and string_of_matchlist = function
    ValMatchList(l) -> " byvalue  {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_expr_or_def (fst p) ^ " {\n" ^ string_of_exprstmtblock (snd p) ^ "}") l) ^ "\n}"
  | TypMatchList(l) -> " bytype {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {\n" ^ string_of_exprstmtblock (snd p) ^ "}") l) ^ "\n}"
and string_of_exprstmt = function
    ExprStmt(e) -> string_of_expr e
and string_of_exprstmtblock l = String.concat "" (List.map (fun e -> string_of_exprstmt e ^ ";\n") l)

let string_of_program exprstmtblock =
    string_of_exprstmtblock exprstmtblock

