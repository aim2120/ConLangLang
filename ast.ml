(* CLL Abstract Syntax Tree and functions for printing it *)

type binop = Mult | Div | Mod | Add | Sub | Concat | And | Or | Equal | Greater | Less

type uop = Neg | Not

type typ = Int | Bool | Float | String | Regex
  | List of typ
  | Dict of typ * typ
  | Fun of typ list * typ
  | UserTyp of string
  | UserTypDef of string

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
  | TypComp of expr * typ
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Cast of typ list * expr
  | ChildAcc of expr * string
  | Assign of string * expr
  | TypDefAssign of typ * string * (string * expr) list
  | Id of string
  | FunCall of expr * expr list
  | Match of mtch
  | IfElse of ifelse
  | While of whle
  | Expr of expr
and funlit = {
    formals: (typ * string) list;
    ftyp: typ;
    fblock: stmt list;
}
and mtch = {
    minput: expr;
    mtyp: typ;
    matchlist: matchlist;
}
and matchlist =
    ValMatchList of (expr_or_def * stmt list) list
  | TypMatchList of (typ_or_def * stmt list) list
and expr_or_def = ExprMatch of expr | DefaultExpr
and ifelse = {
    icond: expr;
    ityp: typ;
    ifblock: stmt list;
    elseblock: stmt list;
}
and whle = {
    wcond: expr;
    wtyp: typ;
    wblock: stmt list;
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
  | Fun(f,t) -> "fun" ^ "<" ^ String.concat ", " (List.map (fun t -> string_of_typ t) f) ^ ":" ^ string_of_typ t ^ ">"
  | UserTyp(u) -> u
  | UserTypDef(u) -> u

let string_of_typ_or_def = function
    TypMatch(t) -> string_of_typ t
  | DefaultTyp -> "default"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> f
  | BoolLit(b) -> if b then "true" else "false"
  | StrLit(s) -> "'" ^ s  ^ "'"
  | ReLit(r) -> "\"" ^ r ^ "\""
  | ListLit(t, l) -> "<" ^ string_of_typ t ^ ">" ^ "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | DictLit(t1, t2, d) -> "<" ^ string_of_typ t1 ^ "," ^ string_of_typ t2 ^ ">" ^ "{" ^
      String.concat "," (List.map (fun p -> string_of_expr (fst p) ^ ":" ^ string_of_expr (snd p)) d) ^ "}"
  | FunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.formals) ^ ":" ^
    string_of_typ f.ftyp ^ ">{" ^ string_of_stmtblock f.fblock ^ "}"
  | TypComp(e,t) -> string_of_expr e ^ "~=" ^ string_of_typ t
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ChildAcc(e, s) -> string_of_expr e ^ "." ^ s
  | Cast(t, e) -> "(" ^ String.concat ", " (List.map string_of_typ t) ^ ")" ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | TypDefAssign(t, v, l) -> string_of_typ t ^ " " ^ v ^ " = {" ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_expr (snd p) ^ ";") l) ^ "}"
  | FunCall(e, l) -> string_of_expr e ^ "(" ^ String.concat ", " (List.map string_of_expr l) ^ ")"
  | Match(m) -> "match:" ^ string_of_typ m.mtyp ^ " (" ^ string_of_expr m.minput ^ ")" ^ string_of_matchlist m.matchlist
  | IfElse(i) -> "if:" ^ string_of_typ i.ityp ^ " (" ^ string_of_expr i.icond ^ ") {" ^
      string_of_stmtblock i.ifblock ^ "} else {" ^ string_of_stmtblock i.elseblock ^ "}"
  | While(w) -> "while:" ^ string_of_typ w.wtyp ^ " (" ^ string_of_expr w.wcond ^ ") {" ^ string_of_stmtblock w.wblock ^ "}"
  | Id(v) -> v
  | Expr(e) -> "(" ^ string_of_expr e ^ ")"
and string_of_expr_or_def = function
    ExprMatch(e) -> string_of_expr e
  | DefaultExpr -> "default"
and string_of_matchlist = function
    ValMatchList(l) -> " byvalue {" ^
    String.concat "" (List.map (fun p -> string_of_expr_or_def (fst p) ^ " {" ^ string_of_stmtblock (snd p) ^ "}") l) ^ "}"
  | TypMatchList(l) -> " bytype {" ^
    String.concat "" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {" ^ string_of_stmtblock (snd p) ^ "}") l) ^ "}"
and string_of_stmt = function
    ExprStmt(e) -> string_of_expr e
  | TypDecl(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | TypDefDecl(v, l) -> "typedef " ^ v ^ " = {" ^
      String.concat ";" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "}"

and string_of_stmtblock l = String.concat "" (List.map (fun e -> string_of_stmt e ^ ";\n") l)

let string_of_program stmtblock =
    string_of_stmtblock stmtblock

