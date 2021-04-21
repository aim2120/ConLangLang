(* Semantic checking for the CLL compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each stmt of program *)

(*
type semantic_env = {
    tvsym: (string * typ) list StringMap.t;
    (* type variable name: list of user type names and definitions *)
    tsym: typ * typ StringMap.t;
    (* user type name: defining type and built-in associate type *)
    tdsym: (typ * string) list StringMap.t;
    (* user typedef name: child declaration list *)
    vsym: typ list StringMap.t;
    (* user variables: all types *)
}
*)

type semantic_env = {
    tvsym: (string * typ) list StringMap.t;
    (* type variable name: list of user type names and definitions *)
    tsym: (typ * typ) StringMap.t;
    (* user type name: defining type and built-in associate type *)
    tdsym: (typ * string) list StringMap.t;
    (* user typedef name: child declaration list *)
    vsym: typ list StringMap.t;
    (* user variables: all types *)
}

let check_ast ast =
    let line_num: int ref = ref 1 in
    let make_err err = raise (Failure ("!!!ERROR!!! line " ^ string_of_int !line_num ^ ": " ^ err)) in
    let built_in_funs_list = [ ("sprint", [(String, "x")], Null); ] in
    let built_in_funs =
        let add_built_in map (id, formals, t) =
            StringMap.add id [Fun(formals, t)] map in
        List.fold_left add_built_in StringMap.empty built_in_funs_list
    in
    let empty_env = {
        tvsym=StringMap.empty;
        tsym=StringMap.empty;
        tdsym=StringMap.empty;
        vsym=built_in_funs;
    }
    in
    let add_env_tvsym env tvsym = {
        tvsym=tvsym;
        tsym=env.tsym;
        tdsym=env.tdsym;
        vsym=env.vsym;
    }
    in
    let add_env_tsym env tsym = {
        tvsym=env.tvsym;
        tsym=tsym;
        tdsym=env.tdsym;
        vsym=env.vsym;
    }
    in
    let add_env_tdsym env tdsym = {
        tvsym=env.tvsym;
        tsym=env.tsym;
        tdsym=tdsym;
        vsym=env.vsym;
    }
    in
    let add_env_vsym env vsym = {
        tvsym=env.tvsym;
        tsym=env.tsym;
        tdsym=env.tdsym;
        vsym=vsym;
    }
    in
    let find_in_map map id err = 
        try StringMap.find id map
        with Not_found -> make_err err
    in
    let to_assc_typ tsym t = match t with
        UserTyp(ut) -> let (_, at) = find_in_map tsym ut (ut ^ " not a defined type")
            in at
        | _ -> t
    in
    let add_typ_var tvsym (id, (l:(string * typ) list)) =
        let already_decl_err = "type variable " ^ id ^ " may not be redefined" in
        match id with
            _ when StringMap.mem id tvsym -> make_err already_decl_err
            | _ -> StringMap.add id l tvsym
    in
    let add_typ tsym (id, (typ:typ)) =
        let already_decl_err = "type " ^ id ^ " may not be redefined" in
        let assc_typ = to_assc_typ tsym typ in
        match id with
            _ when StringMap.mem id tsym -> make_err already_decl_err
            | _ -> StringMap.add id (typ, assc_typ) tsym
    in
    let add_typdef tdsym (id, (l:(typ * string) list)) =
        let already_decl_err = "typedef " ^ id ^ " may not be redefined" in
        match id with
            _ when StringMap.mem id tdsym -> make_err already_decl_err
            | _ -> StringMap.add id l tdsym
    in
    let add_var vsym (id, (typlist:typ list)) = 
        let built_in_err = "std lib function " ^ id ^ " may not be redefined" in
        match id with (* No duplicate functions or redefinitions of built-ins *)
            _ when StringMap.mem id built_in_funs -> make_err built_in_err
            | _ ->  StringMap.add id typlist vsym 
    in
    let rec check_typlist typlist t err = 
        match typlist with
        hd::tl -> if t = hd then t else check_typlist tl t err
        | [] -> make_err err
    in
    let rec check_typlists typlist1 typlist2 err =
        match typlist2 with
        hd::tl -> (try check_typlist typlist1 hd err with Failure(_) -> check_typlists typlist1 tl err)
        | [] -> make_err err
    in
    let check_none s t = match t with
        Null -> make_err (s ^ " cannot be of type none")
        | _ -> ()
    in
    let rec check_valid_typ env t =
        let t = to_assc_typ env.tsym t in
        match t with
            List(t') -> check_valid_typ env t';
            | Dict(t1,t2) ->
                check_valid_typ env t1;
                check_valid_typ env t2;
            | UserTypDef(utd) ->
                let _ = find_in_map env.tdsym utd (utd ^ " not a defined type") in ()
            | _ -> ()
    in
    let check_last_stmt stmts t =
        let last_stmt = List.hd stmts in (* assume already reversed *)
        let (typlist, _) = match last_stmt with
            SExprStmt(s) -> s
            | _ -> make_err "last statement of block must be an expression"
        in
        check_typlist typlist t "last expression of function does not return declared type"
    in
    let rec check_stmt (env, stmts) stmt =
        let output = match stmt with
        ExprStmt(e) ->
            let (e_t, se, vsym) = check_expr env e in
            let env' = add_env_vsym env vsym in
            (env', SExprStmt((e_t, se))::stmts)
        | TypDecl(id, l) ->
            let tvsym = add_typ_var env.tvsym (id, l) in
            let tsym = List.fold_left add_typ env.tsym l in
            let env' = add_env_tvsym (add_env_tsym env tsym) tvsym in
            (env', STypDecl(id, l)::stmts)
        | TypDefDecl(id, l) -> 
            let tdsym = add_typdef env.tdsym (id, l) in
            let env' = add_env_tdsym env tdsym in
            (env', STypDefDecl(id,l)::stmts)
        in
        line_num := !line_num + 1;
        output
    and check_expr env expr = 
        match expr with
        IntLit(i) -> ([Int], SIntLit i, env.vsym)
        | FloatLit(f) -> ([Float], SFloatLit f, env.vsym)
        | BoolLit(b) -> ([Bool], SBoolLit b, env.vsym)
        | StrLit(s) -> ([String], SStrLit s, env.vsym)
        | ReLit(r) -> ([Regex], SReLit r, env.vsym)
        | ListLit(t,l) ->
            check_none "list" t;
            check_valid_typ env t;
            let err = "list literal type inconsistency" in
            let check_list (l, vsym) e =
                let (e_typlist, se, vsym') = check_expr (add_env_vsym env vsym) e in
                let _ = check_typlist e_typlist t err in
                ((e_typlist, se)::l, vsym')
            in
            let (slist, vsym) = List.fold_left check_list ([], env.vsym) l in 
            ([List(t)], SListLit(t,(List.rev slist)), vsym)
        | DictLit(t1,t2,l) ->
            check_none "dictionary key" t1;
            check_none "dictionary value" t2;
            check_valid_typ env t1;
            check_valid_typ env t2;
            let err = "dictionary literal type inconsistency" in
            let check_dict (l', vsym) (e1,e2) =
                let (e1_typlist, e1', vsym') = check_expr (add_env_vsym env vsym) e1 in
                let (e2_typlist, e2', vsym'') = check_expr (add_env_vsym env vsym') e2 in
                let _ = check_typlist e1_typlist t1 err in
                let _ = check_typlist e2_typlist t2 err in
                (((e1_typlist, e1'), (e2_typlist, e2'))::l', vsym'')
            in
            let (slist, vsym) = List.fold_left check_dict ([], env.vsym) l in
            ([Dict(t1,t2)], SDictLit(t1, t2, List.rev slist), vsym)
        | FunLit(f) ->
            check_valid_typ env f.ftyp;
            let check_formal vsym (t, id) =
                check_none "formal argument" t;
                check_valid_typ env t;
                add_var vsym (id, [t])
            in
            let temp_env = add_env_vsym env (List.fold_left check_formal env.vsym f.formals) in
            let (_, sfblock) = List.fold_left check_stmt (temp_env, []) f.fblock in
            let _ = check_last_stmt sfblock f.ftyp in
            let f' = {
                sformals = f.formals;
                sftyp = f.ftyp;
                sfblock = List.rev sfblock;
            } in
            ([Fun(f'.sformals, f'.sftyp)], SFunLit(f'), env.vsym)
        | Binop(e1,o,e2) ->
            let (typlist1, se1, vsym) = check_expr env e1 in
            let (typlist2, se2, vsym') = check_expr (add_env_vsym env vsym) e2 in
            let e1_at = to_assc_typ env.tsym (List.hd typlist1) in
            let e2_at = to_assc_typ env.tsym (List.hd typlist2) in
            let err = "binary operation requires operands of the same type" in
            let at = if e1_at = e2_at then e1_at else make_err err in
            let typlist = match o with
            Mult | Div | Mod | Add | Sub when at = Int || at = Float -> typlist1
            | Concat when at = String || (match at with List(_) -> true | _ -> false) -> typlist1
            | And | Or when at = Bool -> typlist1
            | Equal | Greater | Less when at = Int || at = Float || at = String -> [Bool]
            | _ -> make_err "binary operation on operands of incorrect type"
            in
            (* binop casts to typlist of 1st operand except with comparison operators *)
            (typlist, SBinop((typlist1, se1),o,(typlist2, se2)), vsym')
        | Unop(o,e) ->
            let (typlist, se, vsym) = check_expr env e in
            let at = to_assc_typ env.tsym (List.hd typlist) in
            (match o with
              Neg when at = Int || at = Float -> ()
            | Not when at = Bool -> ()
            | _ -> make_err "unary operation on operand of incorrect type");
            (typlist, SUnop(o, (typlist, se)), vsym)
        | Cast(l,e) ->
            List.iter (fun t -> check_none "cast" t) l;
            List.iter (fun t -> check_valid_typ env t) l;
            let at = to_assc_typ env.tsym (List.hd l) in
            let (typlist, se, vsym) = check_expr env e in
            let e_at = to_assc_typ env.tsym (List.hd typlist) in
            if at = e_at then
                (l, SCast(l, (typlist, se)), vsym)
            else
                let e_at_int = e_at = Int in
                let e_at_float = e_at = Float in
                let e_at_bool = e_at = Bool in
                let e_at_regex = e_at = Regex in
                (match at with
                    Int when e_at_float -> ()
                    | Float when e_at_int -> ()
                    | String when e_at_int || e_at_float || e_at_bool || e_at_regex -> ()
                    | _ -> make_err "incompatible cast");
                (l, SCast(l, (typlist, se)), vsym)
        | ChildAcc(e,id) ->
            let (typlist, se, vsym) = check_expr env e in
            let t = List.hd typlist in (* typdef expr will only have 1 element in typlist *)
            let ut = match t with
                UserTypDef(ut) -> ut
                | _ -> make_err "attempting access into non-typedef variable"
            in
            let td_children = find_in_map env.tdsym ut (ut ^ " not a defined typedef") in (* should never fail *)
            let rec find x = function
                [] -> make_err "attempting access of undeclared typedef child"
                | hd::tl -> let (child_t,child_id) = hd in if x = child_id then child_t else find x tl
            in
            let child_t = find id td_children in
            ([child_t], SChildAcc((typlist, se), id), vsym)
        | Assign(id,e) ->
            let (typlist, se, vsym) = check_expr env e in
            let vsym' = add_var vsym (id, typlist) in
            (typlist, SAssign(id, (typlist, se)), vsym')
        | TypDefAssign(ut,id,l) ->
            let td_children = find_in_map env.tdsym ut ("typedef " ^ ut ^ " not defined") in
            let check_assignment (vsym, l) (id, e) = 
                let (typlist, se, vsym') = check_expr (add_env_vsym env vsym) e in
                let rec find x = function
                    [] -> make_err ("attempting assignment of undeclared child of typdef " ^ ut)
                    | hd::tl -> let (_, child_id) = hd in if x = child_id then hd else find x tl
                in
                let (child_t, child_id) = find id td_children in
                let t = check_typlist typlist child_t ("incompatible assignment to typedef " ^ ut ^ " child " ^ child_id) in 
                (* if e has multiple types, only keep the type that matches the child declaration *)
                (vsym', (id, ([t], se))::l)
            in
            let (vsym, sl) = List.fold_left check_assignment (env.vsym, []) l in
            let vsym' = add_var vsym (id, [UserTypDef(ut)]) in
            ([UserTypDef(ut)], STypDefAssign(ut, id, List.rev sl), vsym')
        | Id(id) ->
            let typlist = find_in_map env.vsym id (id ^ " not defined") in
            (typlist, SId(id), env.vsym)
        | UTDId(id) ->
            let typlist = find_in_map env.vsym id (id ^ " not defined") in
            (typlist, SUTDId(id), env.vsym)
        | FunCall(id,l) ->
            let typlist = find_in_map env.vsym id (id ^ " not defined") in
            (* fun variable has 1 type *)
            let (formals, typ) = match (List.hd typlist) with
                Fun(formals, typ) -> (formals, typ)
                | _ -> make_err (id ^ " is not a function variable")
            in
            let rec check_args formals actuals sactuals vsym =
                match formals, actuals with
                f_hd::f_tl, a_hd::a_tl ->
                    let (f_t, _) = f_hd in
                    let (a_typlist, a_e, vsym') = check_expr (add_env_vsym env vsym) a_hd in
                    let t = check_typlist a_typlist f_t "function argument type doesn't match formal definition" in
                    (* only keep actual argument typ that matches formal definition *)
                    check_args f_tl a_tl (([t],a_e)::sactuals) vsym'
                | [], [] -> (sactuals, vsym)
                | _, _ -> make_err "function arguments incompatible with function definition"
            in
            let (l', vsym) = check_args formals l [] env.vsym in
            ([typ], SFunCall(id, List.rev l'), vsym)
        | Match(m) ->
            check_valid_typ env m.mtyp;
            let (input_typlist, input_se, vsym) = check_expr env m.minput in
            let smatchlist = match m.matchlist with
                ValMatchList(l) ->
                    let rec find_default = function
                        [] -> make_err "match block has no default block"
                        | (e,_)::tl -> (match e with DefaultExpr -> () | _ -> find_default tl)
                    in find_default l;
                    let check_block blocks (e_or_d, stmts) =
                        let (se_or_d, vsym') = match e_or_d with
                            ExprMatch(e) ->
                                let (e_typlist, se, vsym') = check_expr (add_env_vsym env vsym) e in 
                                let err = "match expression type doesn't match input type" in
                                let t = check_typlists input_typlist e_typlist err in
                                (* only keep type that matches input ahd exprmatch *)
                                (SExprMatch(([t], se)), vsym')
                            | DefaultExpr -> (SDefaultExpr, vsym)
                        in
                        let temp_env = add_env_vsym env vsym' in
                        let (_,sstmts) = List.fold_left check_stmt (temp_env, []) stmts in
                        let _ = check_last_stmt sstmts m.mtyp in
                        (se_or_d, List.rev sstmts)::blocks
                    in let l' = List.fold_left check_block [] l in
                    SValMatchList(List.rev l')
                | TypMatchList(l) ->
                    let rec find_default = function
                        [] -> make_err "match block has no default block"
                        | (t,_)::tl -> (match t with DefaultTyp -> () | _ -> find_default tl)
                    in find_default l;
                    let check_block blocks (t_or_d, stmts) =
                        (match t_or_d with
                            TypMatch(t) -> check_valid_typ env t
                            | DefaultTyp -> ());
                        let temp_env = add_env_vsym env vsym in
                        let (_,sstmts) = List.fold_left check_stmt (temp_env, []) stmts in
                        let _ = check_last_stmt sstmts m.mtyp in
                        (t_or_d, List.rev sstmts)::blocks
                    in let l' = List.fold_left check_block [] l in
                    STypMatchList(List.rev l')
            in
            let m' = {
                sminput = (input_typlist, input_se);
                smtyp = m.mtyp;
                smatchlist = smatchlist;
            } in
            ([m'.smtyp], SMatch(m'), vsym)
        | IfElse(i) ->
            check_valid_typ env i.ityp;
            let (cond_typlist, cond_se, vsym) = check_expr env i.icond in
            let at = to_assc_typ env.tsym (List.hd cond_typlist) in
            (match at with
                Bool -> ()
                | _ -> make_err "if/else condition must be a boolean");
            let temp_env = add_env_vsym env vsym in
            let (_, sifblock) = List.fold_left check_stmt (temp_env, []) i.ifblock in
            let _ = check_last_stmt sifblock i.ityp in
            let (_, selseblock) = List.fold_left check_stmt (temp_env, []) i.elseblock in
            let _ = check_last_stmt selseblock i.ityp in
            let i' = {
                sicond = (cond_typlist, cond_se);
                sityp = i.ityp;
                sifblock = List.rev sifblock;
                selseblock = List.rev selseblock;

            } in
            ([i'.sityp], SIfElse(i'), vsym)
        | While(w) ->
            check_valid_typ env w.wtyp;
            let (cond_typlist, cond_se, vsym) = check_expr env w.wcond in
            let at = to_assc_typ env.tsym (List.hd cond_typlist) in
            (match at with
                Bool -> ()
                | _ -> make_err "while condition must be a boolean");
            let temp_env = add_env_vsym env vsym in
            let (_, sblock) = List.fold_left check_stmt (temp_env, []) w.wblock in
            let _ = check_last_stmt sblock w.wtyp in
            let w' = {
                swcond = (cond_typlist, cond_se);
                swtyp = w.wtyp;
                swblock = List.rev sblock;
            } in
            ([w'.swtyp], SWhile(w'), vsym)
        | Expr(e) ->
            check_expr env e
        | NullExpr -> ([Null], SNullExpr, env.vsym)
    
    in
    let (env, sast) = List.fold_left check_stmt (empty_env, []) ast 
    in
(* 
    let (x, _) = List.fold_left (fun (x, num) y -> print_string ("stmt" ^ string_of_int num); (check_stmt x y, num + 1)) ((empty_env, []), 0) ast
    in
    let sast = snd x in
*)
    (env, List.rev sast)
