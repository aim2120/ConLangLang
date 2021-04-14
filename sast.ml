(* CLL Semantically-checked Abstract Syntax Tree and functions for printing it *)

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
  | SBinop of sexpr * binop * sexpr
  | SUnop of uop * sexpr
  | SCast of typ list * sexpr
  | SChildAcc of sexpr * string
  | SAssign of string * sexpr
  | STypDefAssign of string * string * (string * sexpr) list
  | SId of string
  | SUTDId of string
  | SFunCall of string * sexpr list
  | SMatch of smtch
  | SIfElse of sifelse
  | SWhile of swhle
  | SExpr of sexpr
  | SNull
and sfunlit = {
    sformals: (typ * string) list;
    styp: typ;
    sblock: sstmt list;
}
and smtch = {
    sinput: sexpr;
    styp: typ;
    smatchlist: smatchlist;
}
and smatchlist =
    SValMatchList of (sexpr_or_def * sstmt list) list
  | STypMatchList of (typ_or_def * sstmt list) list
and sexpr_or_def = SExprMatch of sexpr | SDefaultExpr
and sifelse = {
    scond: sexpr;
    styp: typ;
    sifblock: sstmt list;
    selseblock: sstmt list;
}
and swhle = {
    scond: sexpr;
    styp: typ;
    sblock: sstmt list;
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
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStrLit(s) -> "'" ^ String.sub s 1 ((String.length s) - 2)  ^ "'" (* substring removes quotes around string *)
  | SReLit(r) -> "\"" ^ String.sub r 1 ((String.length r) - 2) ^ "\"" (* substring removes quotes around string *)
  | SListLit(t, l) -> "<" ^ string_of_typ t ^ ">" ^ "" ^ String.concat ", " (List.map string_of_sexpr l) ^ "]"
  | SDictLit(t1, t2, d) -> "<" ^ string_of_typ t1 ^ "," ^ string_of_typ t2 ^ ">" ^ "{\n" ^
      String.concat ",\n" (List.map (fun p -> string_of_sexpr (fst p) ^ ":" ^ string_of_sexpr (snd p)) d) ^ "\n}"
  | SFunLit(f) -> "<" ^ String.concat ", " (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) f.sformals) ^ ":" ^
    string_of_typ f.styp ^ ">{\n" ^ string_of_sstmtblock f.sblock ^ "}"
  | SNull -> "null"
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SChildAcc(e, s) -> string_of_sexpr e ^ "." ^ s
  | SCast(t, e) -> "(" ^ String.concat ", " (List.map string_of_typ t) ^ ")" ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | STypDefAssign(t, v, l) -> t ^ " " ^ v ^ " = {\n" ^ String.concat "" (List.map (fun p -> fst p ^ " = " ^ string_of_sexpr (snd p) ^ ";\n") l) ^ "}"
  | SFunCall(v, l) -> v ^ "(" ^ String.concat ", " (List.map string_of_sexpr l) ^ ")"
  | SMatch(m) -> "match:" ^ string_of_typ m.styp ^ " (" ^ string_of_sexpr m.sinput ^ ")" ^ string_of_smatchlist m.smatchlist
  | SIfElse(i) -> "if:" ^ string_of_typ i.styp ^ " (" ^ string_of_sexpr i.scond ^ ") {\n" ^
      string_of_sstmtblock i.sifblock ^ "} else {\n" ^ string_of_sstmtblock i.selseblock ^ "}"
  | SWhile(w) -> "while:" ^ string_of_typ w.styp ^ " (" ^ string_of_sexpr w.scond ^ ") {\n" ^ string_of_sstmtblock w.sblock ^ "}"
  | SId(v) -> v
  | SUTDId(v) -> v
  | SExpr(e) -> "(" ^ string_of_sexpr e ^ ")"
  in "(typs: " ^ String.concat ", " (List.map string_of_typ (fst sexpr)) ^ ")" ^ s
and string_of_sexpr_or_def = function
    SExprMatch(e) -> string_of_sexpr e
  | SDefaultExpr -> "default"
and string_of_smatchlist = function
    SValMatchList(l) -> " byvalue  {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_sexpr_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "\n}"
  | STypMatchList(l) -> " bytype {\n" ^
    String.concat "\n" (List.map (fun p -> string_of_typ_or_def (fst p) ^ " {\n" ^ string_of_sstmtblock (snd p) ^ "}") l) ^ "\n}"
and string_of_sstmt = function
    SExprStmt(e) -> string_of_sexpr e
  | STypDecl(v, l) -> "type " ^ v ^ " = {" ^
      String.concat ", " (List.map (fun p -> fst p ^ "<" ^ string_of_typ (snd p) ^ ">") l) ^ "}"
  | STypDefDecl(v, l) -> "typedef " ^ v ^ " = {\n" ^
      String.concat ";\n" (List.map (fun p -> string_of_typ (fst p) ^ " " ^ snd p) l) ^ "\n}"
and string_of_sstmtblock l = String.concat "" (List.map (fun e -> string_of_sstmt e ^ ";\n") l)

let string_of_sprogram sstmtblock =
    string_of_sstmtblock sstmtblock

