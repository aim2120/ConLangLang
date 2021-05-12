(* CLL Semantically-checked Abstract Syntax Tree and functions for printing it *)
(* Author: Annalise Mariottini *)

open Ast

(* accomodates multiple usertypes *)
type sexpr = typ list * sx
and sx =
    SIntLit of int
  | SFloatLit of string
  | SBoolLit of bool
  | SStrLit of string
  | SReLit of string
  | SListLit of typ * sexpr list
  | SDictLit of typ * typ * (sexpr * sexpr) list
  | SFunLit of sfunlit
  | STypComp of sexpr * typ
  | SBinop of sexpr * binop * sexpr
  | SUnop of uop * sexpr
  | SCast of typ list * sexpr
  | SChildAcc of sexpr * string
  | SAssign of string * sexpr
  | STypDefAssign of typ * string * (string * sexpr) list
  | SId of string
  | SFunCall of sexpr * sexpr list
  | SMatch of smtch
  | SIfElse of sifelse
  | SWhile of swhle
and sfunlit = {
    sformals: (typ * string) list;
    sftyp: typ;
    sfblock: sstmt list;
}
and smtch = {
    sminput: sexpr;
    smtyp: typ;
    smatchlist: smatchlist;
}
and smatchlist =
    SValMatchList of (sexpr_or_def * sstmt list) list
  | STypMatchList of (typ_or_def * sstmt list) list
and sexpr_or_def = SExprMatch of sexpr | SDefaultExpr
and sifelse = {
    sicond: sexpr;
    sityp: typ;
    sifblock: sstmt list;
    selseblock: sstmt list;
}
and swhle = {
    swcond: sexpr;
    swtyp: typ;
    swblock: sstmt list;
}
and sstmt =
    SExprStmt of sexpr
  | STypDecl of string * (string * typ) list
  | STypDefDecl of string * (typ * string) list

type sprogram = sstmt list

(* Pretty-printing functions *)

let rec string_of_sexpr sexpr =
  let s = match snd sexpr with
    SIntLit(i) -> string_of_int i
  | SFloatLit(f) -> f
  | SBoolLit(b) -> if b then "true" else "false"
  | SStrLit(s) -> "'" ^ s  ^ "'" (* substring removes quotes around string *)
  | SReLit(r) -> "\"" ^ r ^ "\"" (* substring removes quotes around string *)
  | SListLit(t, l) -> "<" ^ string_of_typ t ^ ">[" ^ String.concat ", " (List.map string_of_sexpr l) ^ "]"
  | SDictLit(t1, t2, d) -> "<" ^ string_of_typ t1 ^ "," ^ string_of_typ t2 ^ ">" ^ "{" ^
      String.concat "," (List.map (fun p -> string_of_sexpr (fst p) ^ ":" ^ string_of_sexpr (snd p)) d) ^ "}"
  | SFunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.sformals) ^ ":" ^
    string_of_typ f.sftyp ^ ">{\n" ^ string_of_sstmtblock f.sfblock ^ "}"
  | STypComp(e,t) -> string_of_sexpr e ^ "~=" ^ string_of_typ t
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SChildAcc(e, s) -> string_of_sexpr e ^ "." ^ s
  | SCast(t, e) -> "(" ^ String.concat ", " (List.map string_of_typ t) ^ ")" ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | STypDefAssign(t, v, l) -> string_of_typ t ^ " " ^ v ^ " = {" ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_sexpr (snd p) ^ ";") l) ^ "}"
  | SFunCall(e, l) -> string_of_sexpr e ^ "(" ^ String.concat ", " (List.map string_of_sexpr l) ^ ")"
  | SMatch(m) -> "match:" ^ string_of_typ m.smtyp ^ " (" ^ string_of_sexpr m.sminput ^ ")" ^ string_of_smatchlist m.smatchlist
  | SIfElse(i) -> "if:" ^ string_of_typ i.sityp ^ " (" ^ string_of_sexpr i.sicond ^ ") {\n" ^
      string_of_sstmtblock i.sifblock ^ "} else {\n" ^ string_of_sstmtblock i.selseblock ^ "}"
  | SWhile(w) -> "while:" ^ string_of_typ w.swtyp ^ " (" ^ string_of_sexpr w.swcond ^ ") {\n" ^ string_of_sstmtblock w.swblock ^ "}"
  | SId(v) -> v
  in "(typs: " ^ String.concat ", " (List.map string_of_typ (fst sexpr)) ^ ")" ^ s
and string_of_sexpr_or_def = function
    SExprMatch(e) -> string_of_sexpr e
  | SDefaultExpr -> "default"
and string_of_smatchlist = function
    SValMatchList(l) -> " byvalue {" ^
    String.concat "" (List.map (fun p -> "\n" ^ string_of_sexpr_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "}"
  | STypMatchList(l) -> " bytype {" ^
    String.concat "" (List.map (fun p -> "\n" ^ string_of_typ_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "}"
and string_of_sstmt = function
    SExprStmt(e) -> string_of_sexpr e
  | STypDecl(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | STypDefDecl(v, l) -> "typedef " ^ v ^ " = {" ^
      String.concat ";" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "}"
and string_of_sstmtblock l = String.concat "" (List.map (fun e -> string_of_sstmt e ^ ";\n") l)

let string_of_sprogram sstmtblock =
    string_of_sstmtblock sstmtblock

