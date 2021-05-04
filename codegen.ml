(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Semant
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (env : semantic_env) (sast : sstmt list)  =
    (* Create the LLVM compilation module into which
       we will generate code *)
    let context = L.global_context () in
    let the_module = L.create_module context "ConLangLang" in

    (* to store different types of lists/dicts *)
    let list_typs : (string, L.lltype) Hashtbl.t = Hashtbl.create 10 in
    let dict_typs : (string, L.lltype) Hashtbl.t = Hashtbl.create 10 in
    let ut_typs   : (string, L.lltype) Hashtbl.t = Hashtbl.create 10 in
    let utd_typs = Hashtbl.create 10 in
    let fun_name_i :int ref = ref 0 in

    (* Get types from the context *)
    let i32_t         = L.i32_type    context
    in let i64_t      = L.i64_type    context
    in let i1_t       = L.i1_type     context
    in let i8_t       = L.i8_type     context
    in let float_t    = L.double_type context
    in let string_t   = L.pointer_type i8_t
    in let void_t     = L.void_type   context
    in

    (* matches struct in lib/linked_list.c *)
    let ll_node = L.named_struct_type context "ll_node" in
    L.struct_set_body ll_node [|string_t; L.pointer_type ll_node|] false;

    (* matches structs in  lib/hash_table.c  *)
    let ht_entry = L.named_struct_type context "entry_s"
    in L.struct_set_body ht_entry [|string_t; string_t; L.pointer_type ht_entry|] false;
    let ht_t = L.named_struct_type context "hashtable_s"
    in L.struct_set_body ht_t [|i32_t; i32_t; L.pointer_type (L.pointer_type ht_entry); i1_t|] false;

    (* convenient numbers/strings *)
    let zero = L.const_int i32_t 0 in
    let one = L.const_int i32_t 1 in
    let two = L.const_int i32_t 2 in
    let not_impl = " not implemented" in
    let internal_err = "internal error" in

    (* Return the LLVM type for a MicroC type *)
    let str_of_ltyp ltyp =
        let janky_str t = List.hd (String.split_on_char ' ' (L.string_of_lltype t)) in
        (match L.classify_type ltyp with
            L.TypeKind.Struct -> (match L.struct_name ltyp with
                Some n -> n
                | None -> janky_str ltyp
            )
            | _ -> janky_str ltyp
        )
    in
    let rec ltyp_of_typ = function
          A.Int    -> i32_t
        | A.Bool   -> i1_t
        | A.Float  -> float_t
        | A.String -> string_t
        | A.List(t)->
            (*
             * struct list { t1 *dummy; ll_node *head; }
             * dummy value is to keep track of actual values in list
             * because ll_node can only accomodate string_t
             *)
            let ltyp = ltyp_of_typ t in
            let ltyp_s = str_of_ltyp ltyp in
            let list_name = "list" ^ ltyp_s in
            L.pointer_type (if Hashtbl.mem list_typs list_name then
                Hashtbl.find list_typs list_name
            else (
                let list_t = L.named_struct_type context list_name in
                L.struct_set_body list_t [|(L.pointer_type ltyp); (L.pointer_type ll_node)|] false;
                Hashtbl.add list_typs list_name list_t;
                list_t
            ))
        | A.Dict(t1,t2) ->
            (*
             * struct dict { t1 *dummy; t2 *dummy; ht_t *table; }
             * dummy values are to keep track of actual values in table
             * becuase ht_t can only accomodate string_t
             *)
            let ltyp1 = ltyp_of_typ t1 in
            let ltyp2 = ltyp_of_typ t2 in
            let ltyp_s = (str_of_ltyp ltyp1) ^ (str_of_ltyp ltyp2) in
            let dict_name = "dict" ^ ltyp_s in
            L.pointer_type (if Hashtbl.mem dict_typs dict_name then
                Hashtbl.find dict_typs dict_name
            else (
                let dict_t = L.named_struct_type context dict_name in
                L.struct_set_body dict_t [|(L.pointer_type ltyp1); (L.pointer_type ltyp2); (L.pointer_type ht_t)|] false;
                Hashtbl.add dict_typs dict_name dict_t;
                dict_t
            ))
        | A.Fun(f,t) ->
            let ltyp = ltyp_of_typ t in
            let f_typs = Array.of_list (List.map ltyp_of_typ f) in
            (L.pointer_type (L.function_type ltyp f_typs))
        | A.UserTyp(ut)  -> Hashtbl.find ut_typs ut
        | A.UserTypDef(utd) -> L.pointer_type (fst (Hashtbl.find utd_typs utd))
        | _        -> raise (Failure ("type" ^ not_impl))
(* TODO
        | A.Null   -> void_t
        | A.Regex  ->
*)
    in

    (* Creating top-level function *)
    let main_t = L.function_type i32_t [||] in
    let main = L.define_function "main" main_t the_module in
    let builder = L.builder_at_end context (L.entry_block main) in

    (* variable name -> value table *)
    let var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
    (* func name -> locals table *)
    let func_locals_tbl : (string, (string, L.llvalue) Hashtbl.t) Hashtbl.t = Hashtbl.create 10 in

    (* start external functions *)
    let declare_func (name, ret, args) var_arg =
        let t = if var_arg then L.var_arg_function_type ret args
        else L.function_type ret args
        in
        let func = L.declare_function name t the_module in
        func
    in
    let declare_funcs map (def : string * L.lltype * L.lltype array) =
        let (name, _, _) = def in
        StringMap.add name (declare_func def false) map
    in

    (* string stuff *)
    let printf_func   : L.llvalue = declare_func ("printf", i32_t, [|string_t|]) true in
    let snprintf_func : L.llvalue = declare_func ("snprintf", i32_t, [|string_t; i32_t; string_t|]) true in
    let strcpy_func   : L.llvalue = declare_func ("strcpy", string_t, [|string_t; string_t|]) false in
    let strcat_func   : L.llvalue = declare_func ("strcat", string_t, [|string_t; string_t|]) false in
    let strlen_func   : L.llvalue = declare_func ("strlen", i32_t, [|string_t;|]) false in
    let debug_empty_str_format = L.build_global_stringptr "DEBUG\n" "fmt" builder in
    let debug_str_format = L.build_global_stringptr "DEBUG %s\n" "fmt" builder in
    let debug_str_format1 = L.build_global_stringptr "DEBUG 1 %s\n" "fmt" builder in
    let debug_str_format2 = L.build_global_stringptr "DEBUG 2 %s\n" "fmt" builder in
    let debug_i32_format = L.build_global_stringptr "DEBUG i32 %d\n" "fmt" builder in
    let debug_lu_format = L.build_global_stringptr "DEBUG LU %lu\n" "fmt" builder in
    let str_format = L.build_global_stringptr "%s\n" "fmt" builder in
    let i32_format = L.build_global_stringptr "%d" "fmt" builder in
    let float_format = L.build_global_stringptr "%f" "fmt" builder in

    (* start stdlib functions *)
    (* linked list functions *)
    let ll_create = "ll_create" in
    let ll_push   = "ll_push" in
    let ll_remove = "ll_remove" in
    let ll_next   = "ll_next" in
    let ll_get    = "ll_get" in
    let ll_print  = "ll_print" in
    let ll_size   = "ll_size" in
    let ll_dup    = "ll_dup" in
    let ll_del    = "ll_del" in
    let ll_defs = [
        (ll_create, (L.pointer_type ll_node), [|string_t|]);
        (ll_push, (L.pointer_type ll_node), [|L.pointer_type ll_node; string_t|]);
        (ll_remove, (L.pointer_type ll_node), [|L.pointer_type ll_node;i32_t|]);
        (ll_next, (L.pointer_type ll_node), [|L.pointer_type ll_node|]);
        (ll_get, (string_t), [|L.pointer_type ll_node; i32_t|]);
        (ll_print, i32_t, [|L.pointer_type ll_node|]);
        (ll_size, i32_t, [|L.pointer_type ll_node|]);
        (ll_dup, (L.pointer_type ll_node), [|L.pointer_type ll_node|]);
        (ll_del, void_t, [|L.pointer_type ll_node|]);
    ] in
    let ll_funcs = List.fold_left declare_funcs StringMap.empty ll_defs in
    let ll_create_func = StringMap.find ll_create ll_funcs in
    let ll_push_func = StringMap.find ll_push ll_funcs in
    let ll_remove_func = StringMap.find ll_remove ll_funcs in
    let ll_next_func = StringMap.find ll_next ll_funcs in
    let ll_get_func = StringMap.find ll_get ll_funcs in
    let ll_print_func = StringMap.find ll_print ll_funcs in
    let ll_size_func = StringMap.find ll_size ll_funcs in
    let ll_dup_func = StringMap.find ll_dup ll_funcs in
    let ll_del_func = StringMap.find ll_del ll_funcs in

    (* hash table functions *)
    let ht_create  = "ht_create" in
    let ht_hash    = "ht_hash" in
    let ht_newpair = "ht_newpair" in
    let ht_set     = "ht_set" in
    let ht_get     = "ht_get" in
    let ht_print   = "ht_print" in
    let ht_size    = "ht_size" in
    let ht_defs = [
        (ht_create, (L.pointer_type ht_t), [|i32_t; i1_t|]);
        (ht_hash, i32_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_newpair, (L.pointer_type ht_entry), [|string_t; string_t|]);
        (ht_get, string_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_set, (L.pointer_type ht_t), [|(L.pointer_type ht_t); string_t; string_t|]);
        (ht_print, i32_t, [|(L.pointer_type ht_t)|]);
        (ht_size, i32_t, [|L.pointer_type ht_t|]);
    ] in
    let ht_funcs = List.fold_left declare_funcs StringMap.empty ht_defs in
    let ht_create_func = StringMap.find ht_create ht_funcs in
    let ht_hash_func = StringMap.find ht_hash ht_funcs in
    let ht_newpair_func = StringMap.find ht_newpair ht_funcs in
    let ht_set_func = StringMap.find ht_set ht_funcs in
    let ht_get_func = StringMap.find ht_get ht_funcs in
    let ht_print_func = StringMap.find ht_print ht_funcs in
    let ht_size_func = StringMap.find ht_size ht_funcs in
    (* end stdlib functions *)
    (* end external functions *)

    let rec expr parent_func builder (e : sx) =
        let make_addr (e : L.llvalue) (t : L.lltype) (malloc : bool) (builder : L.llbuilder) =
            let addr = if malloc then L.build_malloc t "" builder else L.build_alloca t "" builder
            in
            (if L.is_null addr then raise (Failure "malloc failed") else ());
            ignore(L.build_store e addr builder);
            addr
        in
        let make_func f_name formals_typs_and_names ret_typ =
            let formals_arr = Array.of_list (List.map (fun (t,_) -> t) formals_typs_and_names) in
            let func_typ = L.function_type ret_typ formals_arr in
            let func = L.define_function f_name func_typ the_module in

            let function_builder = L.builder_at_end context (L.entry_block func) in
            let params = Array.to_list (L.params func) in
            let func_locals = Hashtbl.create (List.length formals_typs_and_names) in
            let make_param p (t,n) =
                L.set_value_name n p;
                let addr = L.build_alloca t "" function_builder in
                ignore(L.build_store p addr function_builder);
                Hashtbl.add func_locals n addr;
            in
            List.iter2 make_param params formals_typs_and_names;
            Hashtbl.add func_locals_tbl f_name func_locals;
            (func, function_builder)
        in

        let load_if_complex e t builder =
            match t with
                A.List(_) | A.Dict(_) ->
                    L.build_load e "load" builder
                | _ -> e
        in

        match e with
        SIntLit(i) -> let out = L.const_int i32_t i in
            (builder, out)
        | SFloatLit(f) -> let out = L.const_float_of_string float_t f in
            (builder, out)
        | SBoolLit(b) -> let out = L.const_int i1_t (if b then 1 else 0) in
            (builder, out)
        | SStrLit(s) ->
            let prefix = "s" in
            let len = String.length s + 1 in
            let addr = L.build_array_malloc i8_t (L.const_int i32_t len) "string" builder in
            (if L.is_null addr then raise (Failure "malloc failed") else ());
            let store_char i c =
                let i' = string_of_int i in
                let c' = Char.code c in
                let addr_i = L.build_in_bounds_gep addr [|L.const_int i32_t i|] (prefix ^ i') builder in
                ignore(L.build_store (L.const_int i8_t c') addr_i builder);
            in
            String.iteri store_char (s ^ "\x00");
            (builder, addr)
        | SListLit(t, l) ->
            let list_t = L.element_type (ltyp_of_typ (A.List(t))) in
            let ltyp = ltyp_of_typ t in

            let addr = L.build_malloc list_t "list" builder in
            (if L.is_null addr then raise (Failure "malloc failed") else ());
            let addr_ltyp = L.build_in_bounds_gep addr [|zero;zero|] "listltyp" builder in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in

            let null_t = L.build_malloc ltyp "null" builder in
            (if L.is_null null_t then raise (Failure "malloc failed") else ());
            ignore(L.build_store (L.const_null ltyp) null_t builder);
            ignore(L.build_store null_t addr_ltyp builder);

            let l' = List.map (fun e -> snd e) l in
            let first = List.hd l' in
            let the_rest = List.tl l' in

            let (builder, first') = expr parent_func builder first in
            let data = make_addr first' ltyp true builder in
            let c_data = L.build_bitcast data string_t "cdata0" builder in
            let first_node = L.build_call ll_create_func [|c_data|] "node0" builder in
            (if L.is_null first_node then raise (Failure "malloc failed") else ());
            ignore(L.build_store first_node addr_head builder);

            let add_node (builder, last_node, i) e =
                let i' = string_of_int i in
                let (builder, e') = expr parent_func builder e in
                let data = make_addr e' ltyp true builder in
                let c_data = L.build_bitcast data string_t ("cdata" ^ i') builder in
                let addr = L.build_call ll_push_func [|last_node; c_data|] ("node" ^ i') builder in
                (if L.is_null addr then raise (Failure "malloc failed") else ());
                (builder, addr, i+1)
            in
            let (builder, _, _) = List.fold_left add_node (builder, first_node, 1) the_rest in
            (builder, addr)
        | SDictLit(t1, t2, d) ->
            let dict_t = L.element_type (ltyp_of_typ (A.Dict(t1,t2))) in
            let ltyp1 = ltyp_of_typ t1 in
            let ltyp2 = ltyp_of_typ t2 in
            let addr = L.build_malloc dict_t "dict" builder in
            (if L.is_null addr then raise (Failure "malloc failed") else ());

            (* dummy values so we know the actual types of the table *)
            let null_t1 = L.build_malloc ltyp1 "nullt1" builder in
            (if L.is_null null_t1 then raise (Failure "malloc failed") else ());
            let null_t2 = L.build_malloc ltyp2 "nullt2" builder in
            (if L.is_null null_t2 then raise (Failure "malloc failed") else ());
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero; one|] "dictt2" builder in
            ignore(L.build_store (L.const_null ltyp1) null_t1 builder);
            ignore(L.build_store (L.const_null ltyp2) null_t2 builder);
            ignore(L.build_store null_t1 addr_t1 builder);
            ignore(L.build_store null_t2 addr_t2 builder);

            (* create dict *)
            let key_is_string = match t1 with A.String -> 1 | _ -> 0 in
            let ht =  L.build_call ht_create_func [|L.const_int i32_t (List.length d);L.const_int i1_t key_is_string|] "tbl" builder in
            (if L.is_null ht then raise (Failure "malloc failed") else ());
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            ignore(L.build_store ht addr_ht builder);

            (* adding all the dict k:v pairs *)
            let d' = List.map (fun (se1, se2) -> (snd se1, snd se2)) d in
            let add_pair (builder, i) (k,v) =
                let i' = string_of_int i in
                let (builder, k') = expr parent_func builder k in
                let (builder, v') = expr parent_func builder v in
                let k_data = make_addr k' ltyp1 true builder in
                let v_data = make_addr v' ltyp2 true builder in
                let c_k_data = L.build_bitcast k_data string_t ("ckdata" ^ i') builder in
                let c_v_data = L.build_bitcast v_data string_t ("cvdata" ^ i') builder in
                let ht = L.build_call ht_set_func [|ht;c_k_data;c_v_data|] "" builder in
                (if L.is_null ht then raise (Failure "malloc failed") else ());
                (builder, i+1)
            in
            let (builder, _) = List.fold_left add_pair (builder, 0) d' in
            (builder, addr)
        | SBinop((typlist,e1), o, (_,e2)) ->
            let t =
                let t = List.hd typlist in
                (match t with
                    A.UserTyp(ut) -> snd (StringMap.find ut env.tsym)
                    | _ -> t)
            in
            let (builder, e1') = expr parent_func builder e1 in
            let (builder, e2') = expr parent_func builder e2 in
            let out = (match t with
                A.Int -> (match o with
                      A.Add     -> L.build_add
                    | A.Sub     -> L.build_sub
                    | A.Mult    -> L.build_mul
                    | A.Div     -> L.build_sdiv
                    | A.Mod     -> L.build_srem
                    | A.Equal   -> L.build_icmp L.Icmp.Eq
                    | A.Greater -> L.build_icmp L.Icmp.Sgt
                    | A.Less    -> L.build_icmp L.Icmp.Slt
                    | _ -> raise (Failure internal_err)
                ) e1' e2' "out" builder
                | A.Float -> (match o with
                      A.Add     -> L.build_fadd
                    | A.Sub     -> L.build_fsub
                    | A.Mult    -> L.build_fmul
                    | A.Div     -> L.build_fdiv
                    | A.Mod     -> L.build_frem
                    | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
                    | A.Greater -> L.build_fcmp L.Fcmp.Ogt
                    | A.Less    -> L.build_fcmp L.Fcmp.Olt
                    | _ -> raise (Failure internal_err)
                ) e1' e2' "out" builder
                | A.Bool -> (match o with
                      A.And -> L.build_and
                    | A.Or  -> L.build_or
                    | _ -> raise (Failure internal_err)
                ) e1' e2' "out" builder
                | A.String -> (match o with
                    A.Concat ->
                        let len1 = L.build_call strlen_func [|e1'|] "e1len" builder in
                        let len2 = L.build_call strlen_func [|e2'|] "e2len" builder in
                        let len = L.build_add len1 len2 "len" builder in
                        let out_addr = L.build_array_malloc i8_t len "out" builder in
                        (if L.is_null out_addr then raise (Failure "malloc failed") else ());
                        let out_addr = L.build_call strcpy_func [|out_addr; e1'|] "cpy" builder in
                        let out_addr = L.build_call strcat_func [|out_addr; e2'|] "cat" builder in
                        out_addr
                    | _ -> raise (Failure internal_err)
                )
                (* TODO
                | A.List -> (match o with
                    A.Concat ->
                    | _ -> raise (Failure internal_err)
                )
                *)
                | _ -> raise (Failure internal_err)
            )
            in
            (builder, out)
        | SUnop(o, (typlist,e)) ->
            let (builder, e') = expr parent_func builder e in
            let t =
                let t = List.hd typlist in
                (match t with
                    A.UserTyp(ut) -> snd (StringMap.find ut env.tsym)
                    | _ -> t)
            in
            let out = (match o with
                A.Not -> L.build_not e' "not" builder
                | A.Neg -> (
                    match t with
                    A.Int -> L.build_neg e' "neg" builder
                    | A.Float -> L.build_fneg e' "neg" builder
                    | _ -> raise (Failure internal_err)
                )
            ) in
            (builder, out)
        | SFunCall((_,SId("lget")), [(_, l); (_,n)]) ->
            let (builder, addr) = expr parent_func builder l in
            let (builder, n') = expr parent_func builder n in
            let f_name = "lget" ^ (str_of_ltyp (L.type_of addr)) in
            let func = (match L.lookup_function f_name the_module with
                Some f -> f
                | None -> (
                    let addr_typ = L.type_of addr in
                    let n_typ = L.type_of n' in
                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" builder in
                    let t = L.type_of (L.build_load (L.build_load addr_t "t*" builder) "t" builder) in
                    let (func, function_builder) = make_func f_name [(addr_typ,"l");(n_typ,"n")] t in

                    let (function_builder, addr) = expr func function_builder (SId("l")) in
                    let (function_builder, k') = expr func function_builder (SId("n")) in

                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" function_builder in
                    let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
                    let ltyp_ptr = L.build_load addr_t "t*" function_builder in
                    let ltyp = L.type_of (L.build_load ltyp_ptr "t" function_builder) in

                    let head_node = L.build_load addr_head "headnode" function_builder in
                    let c_data = L.build_call ll_get_func [|head_node;n'|] "cdata" function_builder in
                    let data = L.build_bitcast c_data (L.type_of ltyp_ptr) "data" function_builder in
                    let data_load = L.build_load data "dataload" function_builder in
                    ignore(L.build_ret data_load function_builder);

                    func
                )
            )
            in
            let data_load = L.build_call func [|addr;n'|] "lget" builder in
            (builder, data_load)
        | SFunCall((_,SId("dget")), [(_, dict); (_,k)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k') = expr parent_func builder k in
            let f_name = "dget" ^ (str_of_ltyp (L.type_of addr)) in
            let func = (match L.lookup_function f_name the_module with
                Some f -> f
                | None -> (
                    let addr_typ = L.type_of addr in
                    let k_typ = L.type_of k' in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
                    let v_typ = L.type_of (L.build_load (L.build_load addr_t2 "t2*" builder) "t2" builder) in
                    let (func, function_builder) = make_func f_name [(addr_typ,"d");(k_typ,"k")] v_typ in

                    (* building dget function *)
                    let (function_builder, addr) = expr func function_builder (SId("d")) in
                    let (function_builder, k')   = expr func function_builder (SId("k")) in

                    let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" function_builder in
                    let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
                    let ltyp1_ptr = L.build_load addr_t1 "t1*" function_builder in
                    let ltyp2_ptr = L.build_load addr_t2 "t2*" function_builder in
                    let ltyp1 = L.type_of (L.build_load ltyp1_ptr "t1" function_builder) in
                    let ltyp2 = L.type_of (L.build_load ltyp2_ptr "t2" function_builder) in
                    let k_data = make_addr k' ltyp1 false function_builder in
                    let c_k_data = L.build_bitcast k_data string_t "ckdata" function_builder in
                    let ht = L.build_load addr_ht "ht" function_builder in
                    let c_v_data = L.build_call ht_get_func [|ht;c_k_data|] "cvdata" function_builder in
                    (*
                     * TODO: make cond branch if return value is null (key not found)
                    let is_null = L.build_is_null c_v_data "is_null" function_builder in
                    *)
                    let v_data = L.build_bitcast c_v_data (L.type_of ltyp2_ptr) "vdata" function_builder in
                    let v_data_load = L.build_load v_data "vdataload" function_builder in
                    ignore(L.build_ret v_data_load function_builder);

                    func
                )
            )
            in
            let v_data_load = L.build_call func [|addr;k'|] "dget" builder in
            (builder, v_data_load)
        | SFunCall((_,SId("dset")), [(_, dict); (_,k); (_,v)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k')   = expr parent_func builder k in
            let (builder, v')   = expr parent_func builder v in
            let f_name = "dset" ^ (str_of_ltyp (L.type_of addr)) in
            let func = (match L.lookup_function f_name the_module with
                Some f -> f
                | None -> (
                    let addr_typ = L.type_of addr in
                    let k_typ = L.type_of k' in
                    let v_typ = L.type_of v' in
                    let (func, function_builder) = make_func f_name [(addr_typ,"d");(k_typ,"k");(v_typ,"v")] addr_typ in

                    (* building dset function *)
                    let (function_builder, addr) = expr func function_builder (SId("d")) in
                    let (function_builder, k')   = expr func function_builder (SId("k")) in
                    let (function_builder, v')   = expr func function_builder (SId("v")) in

                    let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" function_builder in
                    let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
                    let ltyp1 = L.type_of (L.build_load (L.build_load addr_t1 "t1*" function_builder) "t1" function_builder) in
                    let ltyp2 = L.type_of (L.build_load (L.build_load addr_t2 "t2*" function_builder) "t2" function_builder) in

                    let k_data = make_addr k' ltyp1 false function_builder in
                    let v_data = make_addr v' ltyp2 false function_builder in
                    let c_k_data = L.build_bitcast k_data string_t "ckdata" function_builder in
                    let c_v_data = L.build_bitcast v_data string_t "cvdata" function_builder in
                    let ht = L.build_load addr_ht "ht" function_builder in
                    let ht' = L.build_call ht_set_func [|ht;c_k_data;c_v_data|] "ht_" function_builder in
                    (if L.is_null ht' then raise (Failure "malloc failed") else ());
                    ignore(L.build_store ht' addr_ht function_builder);
                    ignore(L.build_ret addr function_builder);
                    func
                )
            )
            in
            let addr = L.build_call func [|addr;k';v'|] "dset" builder in
            (builder, addr)
        | SFunCall((_,SId("ssize")), [(_, s)]) ->
            let (builder, addr) = expr parent_func builder s in
            let size = L.build_call strlen_func [|addr|] "ssize" builder in
            (builder, size)
        | SFunCall((_,SId("lsize")), [(_, l)]) ->
            let (builder, addr) = expr parent_func builder l in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in
            let head_node = L.build_load addr_head "headnode" builder in
            let size = L.build_call ll_size_func [|head_node|] "lsize" builder in
            (builder, size)
        | SFunCall((_,SId("dsize")), [(_,d)]) ->
            let (builder, addr) = expr parent_func builder d in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let size = L.build_call ht_size_func [|ht|] "dsize" builder in
            (builder, size)
        | SFunCall((_,SId("sprint")), [(_,e)]) ->
            let (builder, e') = expr parent_func builder e in
            let out = L.build_call printf_func [|str_format;e'|] "printf" builder in
            (builder, out)
        | SFunCall((_,SId("lprint")), [(_,e)]) ->
            let (builder, addr) = expr parent_func builder e in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in
            let head_node = L.build_load addr_head "headnode" builder in
            let out = L.build_call ll_print_func [|head_node|] "printlist" builder in
            (builder, out)
        | SFunCall((_,SId("dprint")), [(_,e)]) ->
            let (builder, addr) = expr parent_func builder e in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let out = L.build_call ht_print_func [|ht|] "printdict" builder in
            (builder, out)
        | SFunCall((_,SId("sfold")), [(_,f);(_,a);(_,s)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, a') = expr parent_func builder a in
            let (builder, addr) = expr parent_func builder s in
            let accum_typ = L.type_of a' in
            let f_name = "sfold" ^ (str_of_ltyp accum_typ) in
            let func = (match L.lookup_function f_name the_module with
                Some f -> f
                | None -> (
                    let arg_func_typ = L.type_of arg_func in
                    let (func, function_builder) = make_func f_name [(arg_func_typ,"f");(accum_typ,"a");(string_t,"s")] accum_typ in

                    let (function_builder, arg_func) = expr func function_builder (SId("f")) in
                    let (function_builder, a') = expr func function_builder (SId("a")) in
                    let (function_builder, addr) = expr func function_builder (SId("s")) in

                    let i_addr = L.build_alloca i32_t "iaddr" function_builder in
                    ignore(L.build_store zero i_addr function_builder);
                    let accum_addr = L.build_malloc (L.type_of a') "accum" function_builder in
                    ignore(L.build_store a' accum_addr function_builder);
                    let len = L.build_call strlen_func [|addr|] "len" function_builder in

                    let cond_bb = L.append_block context "cond" func in
                    let cond_builder = L.builder_at_end context cond_bb in
                    let i = L.build_load i_addr "i" cond_builder in
                    let cond = L.build_icmp L.Icmp.Slt i len "lessthan" cond_builder in

                    let body_bb = L.append_block context "foldbody" func in
                    let body_builder = L.builder_at_end context body_bb in
                    let c_str = L.build_array_malloc i8_t two "cstr" body_builder in
                    let c_char = L.build_in_bounds_gep c_str [|zero|] "cchar" body_builder in
                    let c_null = L.build_in_bounds_gep c_str [|one|] "cnull" body_builder in
                    ignore(L.build_store (L.const_int i8_t 0) c_null body_builder);
                    let c_addr = L.build_in_bounds_gep addr [|i|] "caddr" body_builder in
                    let c = L.build_load c_addr "c" body_builder in
                    ignore(L.build_store c c_char body_builder);
                    let a = L.build_load accum_addr "accumload" body_builder in
                    let a = L.build_call arg_func [|a;c_str|] "accumresult" body_builder in
                    ignore(L.build_store a accum_addr body_builder);
                    let i = L.build_add i one "i" body_builder in
                    ignore(L.build_store i i_addr body_builder);

                    let merge_bb = L.append_block context "merge" func in

                    ignore(L.build_br cond_bb function_builder);
                    ignore(L.build_br cond_bb body_builder);
                    ignore(L.build_cond_br cond body_bb merge_bb cond_builder);

                    let function_builder = L.builder_at_end context merge_bb in
                    let accum_final = L.build_load accum_addr "accumfinal" function_builder in

                    ignore(L.build_ret accum_final function_builder);

                    func
                )
            ) in
            let accum_final = L.build_call func [|arg_func;a';addr|] "accumfinal" builder in
            (builder, accum_final)
        | SFunCall((_,SId("lfold")), [(_,f);(_,a);(_,l)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, a') = expr parent_func builder a in
            let (builder, addr) = expr parent_func builder l in
            let accum_typ = L.type_of a' in
            let f_name = "lfold" ^ (str_of_ltyp accum_typ) in
            let func = (match L.lookup_function f_name the_module with
                Some f -> f
                | None -> (
                    let arg_func_typ = L.type_of arg_func in
                    let addr_typ = L.type_of addr in
                    let (func, function_builder) = make_func f_name [(arg_func_typ,"f");(accum_typ,"a");(addr_typ,"l")] accum_typ in

                    let (function_builder, arg_func) = expr func function_builder (SId("f")) in
                    let (function_builder, a') = expr func function_builder (SId("a")) in
                    let (function_builder, addr) = expr func function_builder (SId("l")) in

                    let i_addr = L.build_alloca i32_t "iaddr" function_builder in
                    ignore(L.build_store zero i_addr function_builder);
                    let accum_addr = L.build_malloc (L.type_of a') "accum" function_builder in
                    ignore(L.build_store a' accum_addr function_builder);

                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listltyp" function_builder in
                    let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
                    let head_node = L.build_load addr_head "headnode" function_builder in
                    let curr_node = L.build_malloc (L.pointer_type ll_node) "currnode" function_builder in
                    ignore(L.build_store head_node curr_node function_builder);

                    let ltyp_ptr = L.element_type (L.type_of addr_t) in
                    let len = L.build_call ll_size_func [|head_node|] "len" function_builder in

                    let cond_bb = L.append_block context "cond" func in
                    let cond_builder = L.builder_at_end context cond_bb in
                    let i = L.build_load i_addr "i" cond_builder in
                    let cond = L.build_icmp L.Icmp.Slt i len "lessthan" cond_builder in

                    let body_bb = L.append_block context "foldbody" func in
                    let body_builder = L.builder_at_end context body_bb in

                    let node = L.build_load curr_node "node" body_builder in
                    let c_data = L.build_call ll_get_func [|node;zero|] "cdata" body_builder in
                    let data = L.build_bitcast c_data ltyp_ptr "data" body_builder in
                    let data_load = L.build_load data "dataload" body_builder in

                    let a = L.build_load accum_addr "accumload" body_builder in
                    let a = L.build_call arg_func [|a;data_load|] "accumresult" body_builder in
                    ignore(L.build_store a accum_addr body_builder);

                    let i = L.build_add i one "i" body_builder in
                    ignore(L.build_store i i_addr body_builder);

                    let next_node = L.build_call ll_next_func [|node|] "nextnode" body_builder in
                    ignore(L.build_store next_node curr_node body_builder);

                    let merge_bb = L.append_block context "merge" func in

                    ignore(L.build_br cond_bb function_builder);
                    ignore(L.build_br cond_bb body_builder);
                    ignore(L.build_cond_br cond body_bb merge_bb cond_builder);

                    let function_builder = L.builder_at_end context merge_bb in
                    let accum_final = L.build_load accum_addr "accumfinal" function_builder in

                    ignore(L.build_ret accum_final function_builder);

                    func
                )
            ) in
            let accum_final = L.build_call func [|arg_func;a';addr|] "accumfinal" builder in
            (builder, accum_final)

        | SAssign(v, (_,e)) ->
            let (builder, e') = expr parent_func builder e in
            Hashtbl.add var_tbl v e';
            (builder, e')
        | SId(v) ->
            let func = L.value_name parent_func in
            let e' = try
                    let locals = Hashtbl.find func_locals_tbl func in
                    let out = Hashtbl.find locals v in
                    L.build_load out "local" builder
                with Not_found -> (
                    try
                        Hashtbl.find var_tbl v
                    with Not_found -> (
                        match L.lookup_function v the_module with
                            Some f -> f
                            | None -> (
                                raise (Failure ("ID not found: " ^ v))
                            )
                    )
                )
            in
            (builder, e')

        | SUTDId(v) ->
            let e' = Hashtbl.find var_tbl v in
            (builder, e')
        | SMatch(m) ->
            let merge_bb = L.append_block context "merge" parent_func in
            let build_br_merge = L.build_br merge_bb in
            let out_addr = L.build_alloca (ltyp_of_typ m.smtyp) "out" builder in

            let make_block (blocks, i) (_, block) =
                let bb = L.append_block context ("block" ^ string_of_int i) parent_func in
                let (_, bb_builder, bb_out) = List.fold_left stmt (parent_func, L.builder_at_end context bb, zero) block in
                ignore(L.build_store bb_out out_addr bb_builder);
                ignore(build_br_merge bb_builder);
                (bb::blocks, i + 1)
            in

            let make_cond_block (blocks, i) (sexpr_or_def, _) then_block =
                let cond_bb = L.append_block context ("cond" ^ string_of_int i) parent_func in
                let cond_builder = L.builder_at_end context cond_bb in
                ignore(match sexpr_or_def with
                    SExprMatch e ->
                        let (cond_builder, cond) = expr parent_func cond_builder (SBinop(e, A.Equal, m.sminput)) in
                        ignore(L.build_cond_br cond then_block (List.hd blocks) cond_builder);
                    | SDefaultExpr ->
                        ignore(L.build_br then_block cond_builder);
                );
                (cond_bb::blocks, i - 1)
            in

            let cond_blocks = (match m.smatchlist with
                SValMatchList(l)->
                    let (blocks, _) = List.fold_left make_block ([], 0) l in
                    (* blocks is in reverse order *)
                    let (cond_blocks, _) = List.fold_left2 make_cond_block ([], (List.length blocks - 1)) (List.rev l) blocks
                    in
                    cond_blocks
                | STypMatchList(l) -> raise (Failure ("typmatch" ^ not_impl))
                (* TODO: implement typmatch *)
            ) in

            ignore(L.build_br (List.hd cond_blocks) builder);
            let builder = L.builder_at_end context merge_bb in
            let out = L.build_load out_addr "matchout" builder in
            (builder, out)
        | SIfElse(i) ->
            let (builder, cond) = expr parent_func builder (snd i.sicond) in
            let merge_bb = L.append_block context "merge" parent_func in
            let build_br_merge = L.build_br merge_bb in
            let out_addr = L.build_alloca (ltyp_of_typ i.sityp) "out" builder in

            let then_bb = L.append_block context "then" parent_func in
            let (_, if_builder, ifout) = List.fold_left stmt (parent_func, L.builder_at_end context then_bb, zero) i.sifblock in
            ignore(L.build_store ifout out_addr if_builder);
            ignore(build_br_merge if_builder);

            let else_bb = L.append_block context "else" parent_func in
            let (_, else_builder, elseout) = List.fold_left stmt (parent_func, L.builder_at_end context else_bb, zero) i.selseblock in
            ignore(L.build_store elseout out_addr else_builder);
            ignore(build_br_merge else_builder);

            ignore(L.build_cond_br cond then_bb else_bb builder);
            let builder = L.builder_at_end context merge_bb in
            let out = L.build_load out_addr "ifelseout" builder in
            (builder, out)
        | SWhile(w) ->
            let ltyp = ltyp_of_typ w.swtyp in
            let out_addr = L.build_alloca ltyp "out" builder in
            (* null value if while doesn't run *)
            ignore(L.build_store (L.const_null ltyp) out_addr builder);

            let cond_bb = L.append_block context "while" parent_func in
            ignore(L.build_br cond_bb builder);
            let (cond_builder, cond) = expr parent_func (L.builder_at_end context cond_bb) (snd w.swcond) in

            let body_bb = L.append_block context "while_body" parent_func in
            let (_, body_builder, body_out) = List.fold_left stmt (parent_func, L.builder_at_end context body_bb, zero) w.swblock in
            ignore(L.build_store body_out out_addr body_builder);
            ignore(L.build_br cond_bb body_builder);

            let merge_bb = L.append_block context "merge" parent_func in
            ignore(L.build_cond_br cond body_bb merge_bb cond_builder);
            let builder = L.builder_at_end context merge_bb in
            let out = L.build_load out_addr "whileout" builder in
            (builder, out)
        | SFunLit(f) ->
            let f_name = "fun" ^ (string_of_int !fun_name_i) in
            fun_name_i := !fun_name_i + 1;
            let formals_typs_and_names = List.map (fun (t,n) -> (ltyp_of_typ t, n)) f.sformals in
            let ret_typ = ltyp_of_typ f.sftyp in
            let (func, function_builder) = make_func f_name formals_typs_and_names ret_typ in
            let (_, function_builder, function_out) = List.fold_left stmt (func, function_builder, zero) f.sfblock in
            ignore(L.build_ret function_out function_builder);
            (builder, func)
        | SFunCall((typlist,e), l) ->
            let make_actuals (builder, actuals) (_, e) =
                let (builder, e') = expr parent_func builder e in
                (builder, e'::actuals)
            in
            let (builder, actuals) = List.fold_left make_actuals (builder, []) l in
            let (builder, func) = expr parent_func builder e in
            let actuals_arr = Array.of_list (List.rev actuals) in
            let out = L.build_call func actuals_arr "funcall" builder in
            (builder, out)
        | SCast(t, (_,e)) ->
            let (builder, e') = expr parent_func builder e in
            let to_string fmt =
                let len = L.build_call snprintf_func [|L.const_null string_t;zero;fmt;e'|] "" builder
                in
                let len = L.build_add len one "" builder in
                let addr = L.build_array_malloc i8_t len "strcast" builder in
                ignore(L.build_call snprintf_func [|addr;len;fmt;e'|] "" builder);
                addr
            in
            let e_ltyp = L.type_of e' in
            let c_ltyp = ltyp_of_typ (List.hd t) in
            let e_cast =
                if c_ltyp = i32_t then (
                    if e_ltyp = float_t then
                        L.build_fptosi e' c_ltyp "intcast" builder
                    else e'
                )
                else if c_ltyp = float_t then (
                    if e_ltyp = i32_t then
                        L.build_sitofp e' c_ltyp "floatcast" builder
                    else e'
                )
                else if c_ltyp = string_t then (
                    if e_ltyp = i32_t then
                        to_string i32_format
                    else if e_ltyp = i1_t then
                        to_string i32_format
                    else if e_ltyp = float_t then
                        to_string float_format
                    else e'
                ) else (
                    e'
                )
            in
            (builder, e_cast)
        | STypDefAssign(t, v, l) ->
            let (utd_typ, name_pos) = Hashtbl.find utd_typs t in
            let name = match L.struct_name utd_typ with Some n -> n | None -> raise (Failure internal_err) in
            let addr = L.build_alloca utd_typ (name ^ v) builder in
            let fill_struct builder (n, (_,e)) =
                let (builder, e') = expr parent_func builder e in
                let pos = Hashtbl.find name_pos n in
                let addr_pos = L.build_in_bounds_gep addr [|zero;L.const_int i32_t pos|] ("tdassign_" ^ n) builder
                in
                ignore(L.build_store e' addr_pos builder);
                builder
            in
            let builder = List.fold_left fill_struct builder l in
            Hashtbl.add var_tbl v addr;
            (builder, addr)
        | SChildAcc((_,e), s) ->
            let (builder, e') = expr parent_func builder e in
            let utd_typ = L.element_type (L.type_of e') in
            let name = match L.struct_name utd_typ with Some n -> n | None -> raise (Failure internal_err) in
            let (_, name_pos) = Hashtbl.find utd_typs name in
            let pos = Hashtbl.find name_pos s in
            let addr_pos = L.build_in_bounds_gep e' [|zero;L.const_int i32_t pos|] (name ^ "." ^ s) builder in
            let out = L.build_load addr_pos ("load" ^ s) builder in
            (builder, out)
        | _ -> raise (Failure ("expr" ^ not_impl))
        (* TODO
        | SReLit(r) -> ()
        | SNullExpr -> ()
                    *)
    and stmt (func, builder, out) = function
        SExprStmt(e) ->
            let (builder', out') = expr func builder (snd e) in
            (func, builder', out')
        | STypDecl(v, l) ->
            let make_typ (n, t) =
                Hashtbl.add ut_typs n (ltyp_of_typ t);
            in
            List.iter make_typ l;
            (func, builder, out)
        | STypDefDecl(v, l) ->
            let td = L.named_struct_type context v in
            let arr = Array.of_list (List.map (fun (t,_) -> ltyp_of_typ t) l) in
            let name_pos : (string, int) Hashtbl.t = Hashtbl.create 10 in
            List.iteri (fun i (_,n) -> Hashtbl.add name_pos n i) l;
            L.struct_set_body td arr false;
            Hashtbl.add utd_typs v (td, name_pos);
            (func, builder, out)
    in
    let (_, builder, _) = List.fold_left stmt (main, builder, zero) sast in
    ignore(L.build_ret (L.const_int i32_t 0) builder);
    the_module
