(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex | List | Dict | Fun | UserTyp of string | UserTypDef of string

type typ_out = IntOut | BoolOut | FloatOut | StringOut | RegexOut
  | ListOut of typ_out
  | DictOut of typ_out * typ_out
  | FunOut of (typ_out * string) list * typ_or_none
  | UserTypOut of string
  | UserTypDefOut of string
and typ_or_none = TypOut of typ_out | None

type typ_or_def = TypMatch of typ_out | DefaultTyp

type expr =
    IntLit of int
  | FloatLit of string
  | BoolLit of bool
  | StrLit of string
  | ReLit of string
  | ListLit of typ_out * expr list
  | DictLit of typ_out * typ_out * (expr * expr) list
  | FunLit of funlit
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Cast of typ_out * expr
  | ChildAcc of expr * string
  | Assign of typ * string * expr
  | ReAssign of string * expr
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
    formals: (typ_out * string) list;
    typ: typ_or_none;
    block: stmt list;
}
and mtch = {
    input: expr;
    typ: typ_or_none;
    matchlist: matchlist;
}
and matchlist =
    ValMatchList of (expr_or_def * stmt list) list
  | TypMatchList of (typ_or_def * stmt list) list
and expr_or_def = ExprMatch of expr | DefaultExpr
and ifelse = {
    cond: expr;
    typ: typ_or_none;
    ifblock: stmt list;
    elseblock: stmt list;
}
and whle = {
    cond: expr;
    typ: typ_or_none;
    block: stmt list;
}
and stmt =
    ExprStmt of expr
  | TypDecl of string * (string * typ_out) list
  | TypDefDecl of string * (typ_out * string) list

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

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Regex -> "regex"
  | List -> "list"
  | Dict -> "dict"
  | Fun -> "fun"
  | UserTyp(u) -> u
  | UserTypDef(u) -> u

let rec string_of_typ_out = function
    IntOut -> "int"
  | BoolOut -> "bool"
  | FloatOut -> "float"
  | StringOut -> "string"
  | RegexOut -> "regex"
  | ListOut(t) -> "list" ^ "<" ^ string_of_typ_out t ^ ">"
  | DictOut(t1,t2) -> "dict" ^ "<" ^ string_of_typ_out t1 ^ "," ^ string_of_typ_out t2 ^ ">"
  | FunOut(f,t) -> "fun" ^ "<" ^ String.concat ", " (List.map (fun p -> string_of_typ_out (fst p) ^ " " ^ snd p) f) ^ ":" ^ string_of_typ_or_none t ^ ">"
  | UserTypOut(u) -> u
  | UserTypDefOut(u) -> u
and string_of_typ_or_none = function
    TypOut(t) -> string_of_typ_out t
  | None -> "none"

let string_of_typ_or_def = function
    TypMatch(t) -> string_of_typ_out t
  | DefaultTyp -> "default"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> "'" ^ String.sub s 1 ((String.length s) - 2)  ^ "'" (* substring removes quotes around string *)
  | ReLit(r) -> "\"" ^ String.sub r 1 ((String.length r) - 2) ^ "\"" (* substring removes quotes around string *)
  | ListLit(t, l) -> "<" ^ string_of_typ_out t ^ ">" ^ "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictLit(t1, t2, d) -> "<" ^ string_of_typ_out t1 ^ "," ^ string_of_typ_out t2 ^ ">" ^ "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "\n}"
  | FunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ_out (fst p) ^ " " ^ snd p) f.formals) ^ ":" ^
    string_of_typ_or_none f.typ ^ ">{\n" ^ string_of_stmtblock f.block ^ "}"
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(e, s) -> string_of_expr e ^ "." ^ s
  | Cast(t, e) -> "(" ^ string_of_typ_out t ^ ")" ^ string_of_expr e
  | Assign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  | ReAssign(v, e) -> v ^ " = " ^ string_of_expr e
  | TypDefAssign(t, v, l) -> t ^ " " ^ v ^ " = " ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_expr (snd p) ^ ";") l)
  | FunCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Match(m) -> "match:" ^ string_of_typ_or_none m.typ ^ " (" ^ string_of_expr m.input ^ ")" ^ string_of_matchlist m.matchlist
  | IfElse(i) -> "if:" ^ string_of_typ_or_none i.typ ^ " (" ^ string_of_expr i.cond ^ ") {\n" ^
      string_of_stmtblock i.ifblock ^ "} else {\n" ^ string_of_stmtblock i.elseblock ^ "}"
  | While(w) -> "while:" ^ string_of_typ_or_none w.typ ^ " (" ^ string_of_expr w.cond ^ ") {\n" ^ string_of_stmtblock w.block ^ "}"
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
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ_out (snd p) ^ ">") l) ^ "}"
  | TypDefDecl(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ_out (fst p) ^ " " ^ snd p) l) ^ "\n}"

and string_of_stmtblock l = String.concat "" (List.map (fun e -> string_of_stmt e ^ ";\n") l)

let string_of_program stmtblock =
    string_of_stmtblock stmtblock

