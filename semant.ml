(* Semantic checking for the CLL compiler *)
(* Author: Annalise Mariottini (aim2120) *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each stmt of program *)

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
    let add_built_in map (id, formals, t) =
        let new_f = Fun(formals, t) in
        if StringMap.mem id map then
            let already_exists f = (f = new_f) in
            let fun_defs = StringMap.find id map in
            if List.exists already_exists fun_defs then map
            else StringMap.add id (new_f::fun_defs) map
        else
            StringMap.add id [new_f] map
    in
    let built_in_funs =
        let l = [
            ("sprint",
                [String],
                Int);
            ("ssize",
                [String],
                Int);
            ("sfold",
                [Fun([String;String],String);String;String],
                String);
            ("lprint",
                [List(String)],
                Int);
            ("lmem",
                [List(String);String],
                Int);
            ("lget",
                [List(String);Int],
                String);
            ("ladd",
                [List(String);String],
                List(String));
            ("ladd",
                [List(String);String; Int],
                List(String));
            ("lremove",
                [List(String)],
                List(String));
            ("lremove",
                [List(String); Int],
                List(String));
            ("lsize",
                [List(String)],
                Int);
            ("lfold",
                [Fun([String;String],String);String;List(String)],
                String);
            ("lmap",
                [Fun([String],String);List(String)],
                List(String));
            ("dprint",
                [Dict(String,String)],
                Int);
            ("dmem",
                [Dict(String,String); String],
                Bool);
            ("dget",
                [Dict(String,String); String],
                String);
            ("dadd",
                [Dict(String,String); String; String],
                Dict(String,String));
            ("dremove",
                [Dict(String,String); String],
                Dict(String,String));
            ("dsize",
                [Dict(String,String)],
                Int);
            ("dfold",
                [Fun([String;String;String],String);String;Dict(String,String)],
                String);
            ("dmap",
                [Fun([String;String],String);Dict(String,String)],
                Dict(String,String));
            ("dkeys",
                [Dict(String,String)],
                List(String));
            ("rematch",
                [Regex;String],
                Bool);
            ("resub",
                [Regex;String;String;Int],
                String);
        ] in
        List.fold_left add_built_in StringMap.empty l
    in
    let add_built_in_list vsym t =
        let l = [
            ("lprint", [List(t)], Int);
            ("lmem", [List(t);t], Int);
            ("lget", [List(t);Int], t);
            ("ladd", [List(t);t], List(t));
            ("ladd", [List(t);t; Int], List(t));
            ("lremove", [List(t)], List(t));
            ("lremove", [List(t); Int], List(t));
            ("lsize", [List(t)], Int);
        ] in
        List.fold_left add_built_in vsym l
    in
    let add_built_in_dict vsym t1 t2 =
        let l = [
            ("dprint", [Dict(t1,t2)], Int);
            ("dmem", [Dict(t1,t2);t1], Bool);
            ("dget", [Dict(t1,t2);t1], t2);
            ("dadd", [Dict(t1,t2);t1; t2], Dict(t1,t2));
            ("dremove", [Dict(t1,t2); t1], Dict(t1,t2));
            ("dsize", [Dict(t1,t2)], Int);
            ("dkeys", [Dict(t1,t2)], List(t1));
        ] in
        List.fold_left add_built_in vsym l
    in
    let empty_env = {
        tvsym=StringMap.empty;
        tsym=StringMap.empty;
        tdsym=StringMap.empty;
        vsym=(StringMap.add "stdin" [List(String)] built_in_funs);
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
    let rec check_valid_typ env t =
        let t = to_assc_typ env.tsym t in
        match t with
            List(t') ->
                let vsym = check_valid_typ env t' in
                add_built_in_list vsym t'
            | Dict(t1,t2) ->
                let vsym = check_valid_typ env t1 in
                let vsym = check_valid_typ (add_env_vsym env vsym) t2 in
                add_built_in_dict vsym t1 t2
            | UserTypDef(utd) ->
                let _ = find_in_map env.tdsym utd (utd ^ " not a defined type") in
                env.vsym
            | Fun(_,_) -> make_err "funs cannot be a stored/passed datatype"
            | _ -> env.vsym
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
            let vsym = check_valid_typ env t in
            let err = "list literal type inconsistency" in
            let check_list (l, vsym) e =
                let (e_typlist, se, vsym') = check_expr (add_env_vsym env vsym) e in
                let _ = check_typlist e_typlist t err in
                ((e_typlist, se)::l, vsym')
            in
            let (slist, vsym') = List.fold_left check_list ([], vsym) l in 
            let vsym' = add_built_in_list vsym' t in
            ([List(t)], SListLit(t,(List.rev slist)), vsym')
        | DictLit(t1,t2,l) ->
            let vsym = check_valid_typ env t1 in
            let vsym = check_valid_typ (add_env_vsym env vsym) t2 in
            let keys = Hashtbl.create (List.length l) in
            let err = "dictionary literal type inconsistency" in
            let check_dict (l', vsym) (e1,e2) =
                let (e1_typlist, e1', vsym') = check_expr (add_env_vsym env vsym) e1 in
                if Hashtbl.mem keys e1' then make_err "dictionary keys must be unique"
                else Hashtbl.add keys e1' 1;
                let (e2_typlist, e2', vsym'') = check_expr (add_env_vsym env vsym') e2 in
                let _ = check_typlist e1_typlist t1 err in
                let _ = check_typlist e2_typlist t2 err in
                (((e1_typlist, e1'), (e2_typlist, e2'))::l', vsym'')
            in
            let (slist, vsym') = List.fold_left check_dict ([], vsym) l in
            let vsym' = add_built_in_dict vsym' t1 t2 in
            ([Dict(t1,t2)], SDictLit(t1, t2, List.rev slist), vsym')
        | FunLit(f) ->
            let vsym = check_valid_typ env f.ftyp in
            let check_formal vsym (t, id) =
                let vsym = check_valid_typ (add_env_vsym env vsym) t in
                add_var vsym (id, [t])
            in
            let temp_env = add_env_vsym env (List.fold_left check_formal vsym f.formals) in
            let (_, sfblock) = List.fold_left check_stmt (temp_env, []) f.fblock in
            let _ = check_last_stmt sfblock f.ftyp in
            let f' = {
                sformals = f.formals;
                sftyp = f.ftyp;
                sfblock = List.rev sfblock;
            } in
            let ret_typ = f'.sftyp in
            let formal_typs = List.map (fun (t,_) -> t) f'.sformals in
            let vsym = (match Fun(formal_typs, ret_typ) with
                Fun([t;String],t') when t = t' ->
                    let vsym = add_built_in vsym ("sfold", [Fun([t;String],t);t;String], t) in
                    add_built_in vsym ("lfold", [Fun([t;String],t);t;List(String)], t)
                | Fun([t;x],t') when t = t' -> (
                    let vsym = add_built_in vsym ("lfold", [Fun([t;x],t);t;List(x)], t) in
                    if x = t' then (
                        add_built_in vsym ("dmap", [Fun([x;x],x);Dict(x,x)], Dict(x,x))
                    ) else vsym
                )
                | Fun([t;x;y],t') when t = t' ->
                    add_built_in vsym ("dfold", [Fun([t;x;y],t);t;Dict(x,y)], t)
                | Fun([t],t') when t = t' -> (
                        add_built_in vsym ("lmap", [Fun([t],t);List(t)], List(t))
                )
                | Fun([k;v],v') when v = v' -> (
                        add_built_in vsym ("dmap", [Fun([k;v],v);Dict(k,v)], Dict(k,v))
                )
                | _ -> vsym
            )
            in
            ([Fun(formal_typs, ret_typ)], SFunLit(f'), vsym)
        | TypComp(e,t) ->
            let err = "type comparison must be of usertype" in
            let check_for_usertyp t =
                (match t with
                    UserTyp(_) -> ()
                    | _ -> make_err err)
            in
            check_for_usertyp t;
            let (typlist, e', vsym) = check_expr env e in
            let vsym' = check_valid_typ (add_env_vsym env vsym) t in
            ([Bool], STypComp((typlist, e'), t), vsym')
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
            let vsym = List.fold_left (fun vsym t -> check_valid_typ (add_env_vsym env vsym) t) env.vsym l in
            let at = to_assc_typ env.tsym (List.hd l) in
            let (typlist, se, vsym) = check_expr (add_env_vsym env vsym) e in
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
            (if id = "stdin" then make_err "cannot redefine stdin variable");
            let (typlist, se, vsym) = check_expr env e in
            let assc_t = to_assc_typ env.tsym (List.hd typlist) in
            let old_assc_t = (try Some (
                let old_typlist = find_in_map vsym id "" in
                (to_assc_typ env.tsym (List.hd old_typlist))
            ) with Failure(_) -> None) in
            (match old_assc_t with
                Some old_assc_t -> (match old_assc_t with
                    Fun(_,_) -> make_err ("cannot redefine function variable " ^ id)
                    | _ when old_assc_t = assc_t -> ()
                    | _ -> make_err ("variable reassignment of " ^ id ^ " must be of the previous type " ^ string_of_typ old_assc_t));
                | None -> ()
            );
            let vsym' = add_var vsym (id, typlist) in
            (typlist, SAssign(id, (typlist, se)), vsym')
        | TypDefAssign(td,id,l) ->
            let rec get_ut = (function
                UserTyp(u) -> get_ut (to_assc_typ env.tsym (UserTyp(u)))
                | UserTypDef(ut) -> ut
                | _ -> make_err "typedef assignment using non-typedef typ"
            ) in
            let ut = get_ut td in
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
            ([UserTypDef(ut)], STypDefAssign(td, id, List.rev sl), vsym')
        | Id(id) ->
            let typlist = find_in_map env.vsym id (id ^ " not defined") in
            (typlist, SId(id), env.vsym)
        | FunCall(e,l) ->
            let make_actuals (vsym, actuals) a =
                let (a_typlist, a_e, vsym') = check_expr (add_env_vsym env vsym) a in
                (vsym', (a_typlist, a_e)::actuals)
            in
            let (vsym, actuals) = List.fold_left make_actuals (env.vsym, []) l in
            let actuals = List.rev actuals in
            let (typlist, se, vsym) = check_expr (add_env_vsym env vsym) e in
            let get_formals_and_ret = function
                Fun(formals, typ) -> (formals, typ)
                | _ -> make_err ("trying to call a non-function expression")
            in
            (* multiple function definitions can exist *)
            let fun_defs = List.map get_formals_and_ret typlist in
            let rec check_args formals actuals = (
                match formals, actuals with
                f_hd::f_tl, a_hd::a_tl ->
                    let (a_typlist, _) = a_hd in
                    ignore(check_typlist a_typlist f_hd "function argument type doesn't match formal definition");
                    (* only keep actual argument typ that matches formal definition *)
                    check_args f_tl a_tl
                | [], [] -> ()
                | _, _ -> make_err "function arguments incompatible with function definition"
            )
            in
            let rec try_def (formals, typ) fun_defs =
                try
                    (check_args formals actuals;
                    ([typ], SFunCall(([Fun(formals,typ)],se), actuals), vsym))
                with Failure(e) -> (match fun_defs with
                    hd::tl -> try_def hd tl
                    | [] -> make_err e)
            in
            try_def (List.hd fun_defs) (List.tl fun_defs) 
        | Match(m) ->
            let vsym = check_valid_typ env m.mtyp in
            let (input_typlist, input_se, vsym) = check_expr (add_env_vsym env vsym) m.minput in
            let smatchlist = match m.matchlist with
                ValMatchList(l) ->
                    let (last_e,_) = List.hd (List.rev l) in
                    (match last_e with
                        DefaultExpr -> ()
                        | _ -> make_err "match block doesn't end with default block");
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
                        let vsym = (match t_or_d with
                            TypMatch(t) -> check_valid_typ env t
                            | DefaultTyp -> env.vsym
                        ) in
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
            let vsym = check_valid_typ env i.ityp in
            let (cond_typlist, cond_se, vsym) = check_expr (add_env_vsym env vsym) i.icond in
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
            let vsym = check_valid_typ env w.wtyp in
            let (cond_typlist, cond_se, vsym) = check_expr (add_env_vsym env vsym) w.wcond in
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
    
    in
    let (env, sast) = List.fold_left check_stmt (empty_env, []) ast 
    in
(* 
    let (x, _) = List.fold_left (fun (x, num) y -> print_string ("stmt" ^ string_of_int num); (check_stmt x y, num + 1)) ((empty_env, []), 0) ast
    in
    let sast = snd x in
*)
    (env, List.rev sast)
