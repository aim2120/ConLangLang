(* CLL Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ_or_none * sx
and sx =
    SIntLit of int
  | SFloatLit of string
  | SBoolLit of bool
  | SStrLit of string
  | SReLit of string
  | SListLit of typ_out * sexpr list
  | SDictLit of typ_out * typ_out * (sexpr * sexpr) list
  | SFunLit of sfunlit
  | SBinop of sexpr * binop * sexpr
  | SUnop of uop * sexpr
  | SCast of typ_out * sexpr
  | SChildAcc of expr * string
  | SAssign of typ * string * sexpr
  | SReAssign of string * sexpr
  | STypDefAssign of string * string * (string * sexpr) list
  | SId of string
  | SFunCall of string * sexpr list
  | SMatch of smtch
  | SIfElse of sifelse
  | SWhile of swhle
  | SExpr of sexpr
  | SNull
and sfunlit = {
    sformals: (typ_out * string) list;
    styp: typ_or_none;
    sblock: sstmt list;
}
and smtch = {
    sinput: sexpr;
    typ: typ_or_none;
    smatchlist: matchlist;
}
and smatchlist =
    SValMatchList of (sexpr_or_def * sstmt list) list
  | STypMatchList of (typ_or_def * sstmt list) list
and sexpr_or_def = SExprMatch of sexpr | SDefaultExpr
and sifelse = {
    scond: sexpr;
    styp: typ_or_none;
    sifblock: sstmt list;
    selseblock: sstmt list;
}
and swhle = {
    scond: sexpr;
    styp: typ_or_none;
    sblock: sstmt list;
}
and sstmt =
    SExprStmt of sexpr
  | STypDecl of string * (string * typ_out) list
  | STypDefDecl of string * (typ_out * string) list

type sprogram = sstmt list

(* Pretty-printing functions *)

let rec string_of_sexpr = function
    SIntLit(i) -> string_of_int i
  | SFloatLit(f) -> f
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStrLit(s) -> "'" ^ String.sub s 1 ((String.length s) - 2)  ^ "'" (* substring removes quotes around string *)
  | SReLit(r) -> "\"" ^ String.sub r 1 ((String.length r) - 2) ^ "\"" (* substring removes quotes around string *)
  | SListLit(t, l) -> "<" ^ string_of_typ_out t ^ ">" ^ "[" ^ String.concat ", " (List.map string_of_sexpr l) ^ "]"
  | SDictLit(t1, t2, d) -> "<" ^ string_of_typ_out t1 ^ "," ^ string_of_typ_out t2 ^ ">" ^ "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_sexpr (fst p) ^ ":" ^ string_of_sexpr (snd p)) d) ^ "\n}"
  | SFunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ_out (fst p) ^ " " ^ snd p) f.sformals) ^ ":" ^
    string_of_typ_or_none f.typ ^ ">{\n" ^ string_of_sstmtblock f.block ^ "}"
  | SNull -> "null"
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SChildAcc(e, s) -> string_of_expr e ^ "." ^ s
  | SCast(t, e) -> "(" ^ string_of_typ_out t ^ ")" ^ string_of_sexpr e
  | SAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr e
  | SReAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | STypDefAssign(t, v, l) -> t ^ " " ^ v ^ " = " ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_sexpr (snd p) ^ ";") l)
  | SFunCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_sexpr l) ^ ")"
  | SMatch(m) -> "match:" ^ string_of_typ_or_none m.typ ^ " (" ^ string_of_sexpr m.input ^ ")" ^ string_of_matchlist m.matchlist
  | SIfElse(i) -> "if:" ^ string_of_typ_or_none i.typ ^ " (" ^ string_of_sexpr i.cond ^ ") {\n" ^
      string_of_sstmtblock i.ifblock ^ "} else {\n" ^ string_of_sstmtblock i.elseblock ^ "}"
  | SWhile(w) -> "while:" ^ string_of_typ_or_none w.typ ^ " (" ^ string_of_sexpr w.cond ^ ") {\n" ^ string_of_sstmtblock w.block ^ "}"
  | SId(v) -> v
  | SExpr(e) -> "(" ^ string_of_sexpr e ^ ")"
and string_of_sexpr_or_def = function
    SExprMatch(e) -> string_of_sexpr e
  | SDefaultExpr -> "default"
and string_of_matchlist = function
    SValMatchList(l) -> " byvalue  {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_sexpr_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "\n}"
  | STypMatchList(l) -> " bytype {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "\n}"
and string_of_sstmt = function
    SExprStmt(e) -> string_of_sexpr e
  | STypDecl(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ_out (snd p) ^ ">") l) ^ "}"
  | STypDefDecl(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ_out (fst p) ^ " " ^ snd p) l) ^ "\n}"
and string_of_sstmtblock l = String.concat "" (List.map (fun e -> string_of_sstmt e ^ ";\n") l)

let string_of_sprogram sstmtblock =
    string_of_sstmtblock sstmtblock

