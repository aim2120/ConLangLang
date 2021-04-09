(* Semantic checking for the CLL compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each stmt of program *)


let checkprogram program =
    let make_err err = raise (Failure err) in
    let built_in_funs_list = [ ("sprint", None, [(String, "x")]); ] in
    let built_in_funs =
        let add_built_in map (name, typ, formals) = StringMap.add name {
            formals = formals;
            typ = typ;
            block = [];
        } map in
        List.fold_left add_built_in StringMap.empty built_in_funs_list
    in
    let add_typ_var map (id, (l:(string * typ_out) list)) =
        let already_decl_err = "typ variable" ^ id ^ " may not be redefined"
        in match id with
            _ when StringMap.mem id map -> make_err already_decl_err
            | _ -> StringMap.add id l map
    in
    let add_typ map (id, typ) =
        let already_decl_err = "type " ^ id ^ " may not be redefined"
        in match id with
            _ when StringMap.mem id map -> make_err already_decl_err
            | _ -> StringMap.add id typ map
    in
    let add_typdef map (id, (l:(typ_out * string) list)) =
        let already_decl_err = "typedef " ^ id ^ " may not be redefined"
        in match id with
            _ when StringMap.mem id map -> make_err already_decl_err
            | _ -> StringMap.add id l map
    in
    let add_var map id typ = 
        let built_in_err = "std lib function " ^ id ^ " may not be redefined"
        in match id with (* No duplicate functions or redefinitions of built-ins *)
            _ when StringMap.mem id built_in_funs -> make_err built_in_err
            | _ ->  StringMap.add id typ map 
    in
    let find_in_map map id err = 
        try StringMap.find id map
        with Not_found -> make_err err
    in
    let same_type t1 t2 err =
        if t1 = t2 then t1 else make_err err
    in
    let rec checkstmt (tvsym, tsym, tdsym, vsym, stmts) stmt =
        match stmt with
        ExprStmt(e) ->
            (tvsym, tsym, tdsym, vsym, SExprStmt(checkexpr (tvsym, tsym, tdsym, vsym) e)::stmts)
        | TypDecl(id, l) ->
            let tvsym = add_typ_var tvsym (id, l) in
            let tsym = List.fold_left add_typ tsym l in
            (tvsym, tsym, tdsym, vsym, STypDecl(id, l)::stmts)
        | TypDefDecl(id, l) -> 
            let tdsym = add_typdef tdsym (id, l) in
            (tvsym, tsym, tdsym, vsym, STypDefDecl(id,l)::stmts)
    and checkexpr (tvsym, tsym, tdsym, vsym) expr = 
        match expr with
        IntLit(i) -> (TypOut(Int), SIntLit i)
        | FloatLit(f) -> (TypOut(Float), SFloatLit f)
        | BoolLit(b) -> (TypOut(Bool), SBoolLit b)
        | StrLit(s) -> (TypOut(String), SStrLit s)
        | ReLit(r) -> (TypOut(Regex), SReLit r)
        | ListLit(t,l) ->
            let err = "list literal type inconsistency" in
            let t = TypOut(t) in
            let checklist l' e =
                let (t', e') = checkexpr e in
                (same_type t t' err, e')::l'
            let l = List.fold_left checklist [] l in
            (t, SListLit (t,(List.rev l)))
        | DictLit(t1,t2,l) ->
            let err = "dict literal type inconsistency" in
            let checkdict l' (e1,e2) =
                let (t1', e1') = checkexpr e1 in
                let (t2', e2') = checkexpr e2 in
                ((same_type TypOut(t1) t1' err, e1'), (same_type TypOut(t2) t2' err, e2'))
            let l = List.fold_left checkdict [] l in
            (TypOut(DictOut(t1,t2)), SDictLit(t1,t2,l))
        | FunLit(f) ->
            let checkformal (t, id) =
                match t with
                UserTypOut(s) -> let _ = find_in_map tsym s (s ^ " not a defined type") in ()
                UserTypDefOut(s) -> let _ = find_in_map tdsym s (s ^ " not a defined typedef") in ()
                _ -> ()
            in
            List.iter checkformal f.formals;
            let (_,_,_,_,sblock) = List.fold_left checkstmt (tvsym, tsym, tdsym, vsym, []) f.block
            in
            let (last_t, _) = List.hd (List.rev sblock) in 
            let err = "last expression of function does not return declared type " ^ f.typ in
            let f = {
                sformals = f.formals;
                styp = same_type f.typ last_t err
                sblock = sblock
            } in
            (TypOut(Fun(f.formals,f.typ)), SFunLit(f))
        | Binop(e1,o,e2) ->
            let (t1, e1) = checkexpr e1 in
            let (t2, e2) = checkexpr e2 in
            let t = same_type t1 t2 "binary operation requires operands of the same type" in
            match o with
                Mult | Div | Mod | Add | Sub when t = TypOut(IntOut)|| t = TypOut(FloatOut) -> ()
                | Concat when t = TypOut(StringOut) -> ()
                | And | Or when t = TypOut(BoolOut) -> ()
                | Equal | Greater | Less when t = TypOut(IntOut) || t = TypOut(FloatOut) || t = TypOut(StringOut) -> ()
                | _ -> make_err "binary operation on operands of incorrect type";
            (t, SBinop((t1,e1),o,(t2,e2))
        | Unop(o,e) ->
            let (t,e) = checkexpr e in
            match o with
                  Neg when t = TypOut(IntOut) || t = TypOut(FloatOut) -> ()
                | Not when t = TypOut(BoolOut) -> ()
                | _ -> make_err "unary operation on operand of incorrect type";
            (t, SUnop(o,(t,e)))
        | Cast(t,e) ->
            let (t',e') = checkexpr e
            if TypOut(t) = t' then
                (t', SCast(t',e'))
            else
                let t'int = t' = TypOut(IntOut) in
                let t'float = t' = TypOut(FloatOut) in
                let t'bool = t' = TypOut(BoolOut) in
                let t'regex = t' = TypOut(RegexOut) in
                match t with
                    UserTypOut(ut) ->
                        let assc_typ = find_in_map tsym ut (ut ^ " not a defined type") in
                        match TypOut(assc_typ) with
                            t' -> ()
                            | TypOut(IntOut) when t'float -> ()
                            | TypOut(FloatOut) when t'int -> ()
                            | TypOut(StringOut) when t'int || t'float || t'bool || t'regex -> ()
                            | _ -> make_err ("incompatible cast to " ^ ut);
                    | IntOut -> when t'float -> ()
                    | FloatOut -> when t'int -> ()
                    | StringOut -> when t'int || t'float || t'bool || t'regex -> ()
                    | _ -> make_err "incompatible cast";
                (TypOut(t), SCast(t',e'))
        | ChildAcc(e,id) of string * expr
            let (t', e') = checkexpr e in
            let tdl = find_in_map tdsym t' "attempting access into non-typedef variable" in
            let rec find x l =
                [] -> make_err "attempting access of undeclared typdef child"
                hd::tl -> let (t,id) = hd in if x = id then t else find x tl
            in
            let t = find id tdl in
            (TypOut(t), SChildAcc((t',e'),id))
            (* left off here *)
            (*
        | Assign(t,id,e) of typ * string * expr
        | ReAssign(id,e) of string * expr
        | TypDefAssign(ut,id,l) of string * string * (string * expr) list
        | Id(id) of string
        | UTDId(id) of string
        | FunCall(id,l) of string * expr list
        | Match(m) of mtch
        | IfElse(i) of ifelse
        | While(w) of whle
        | Expr(e) of expr
        | Null -> (Null, SNull)
        *)
    
    in
    let (_,_,_,_,sast) = List.fold_left checkstmt (StringMap.empty, StringMap.empty, StringMap.empty, built_in_funs, []) program
    in List.rev sast
