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
    let list_types = Hashtbl.create 10 in
    let dict_types = Hashtbl.create 10 in
    let fun_name_i = ref 0 in

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
    in L.struct_set_body ht_t [|i32_t; i32_t; L.pointer_type (L.pointer_type ht_entry)|] false;

    (* convenient numbers/strings *)
    let zero = L.const_int i32_t 0 in
    let one = L.const_int i32_t 1 in
    let two = L.const_int i32_t 2 in
    let not_impl = " not implemented" in

    (* Return the LLVM type for a MicroC type *)
    let ltyp_to_str ltyp = List.hd (String.split_on_char ' ' (L.string_of_lltype ltyp))
    in
    let rec typ_to_ltyp = function
          A.Int    -> i32_t
        | A.Bool   -> i1_t
        | A.Float  -> float_t
        | A.String -> string_t
        | A.Null   -> void_t
        | A.List(t)->
            (*
             * struct list { t1 *dummy; ll_node *head; }
             * dummy value is to keep track of actual values in list
             * because ll_node can only accomodate string_t
             *)
            let ltyp = typ_to_ltyp t in
            let ltyp_s = ltyp_to_str ltyp in
            let list_name = "list" ^ ltyp_s in
            L.pointer_type (if Hashtbl.mem list_types list_name then
                Hashtbl.find list_types list_name
            else (
                let list_t = L.named_struct_type context list_name in
                L.struct_set_body list_t [|(L.pointer_type ltyp); (L.pointer_type ll_node)|] false;
                Hashtbl.add list_types list_name list_t;
                list_t
            ))
        | A.Dict(t1,t2) ->
            (*
             * struct dict { t1 *dummy; t2 *dummy; ht_t *table; }
             * dummy values are to keep track of actual values in table
             * becuase ht_t can only accomodate string_t
             *)
            let ltyp1 = typ_to_ltyp t1 in
            let ltyp2 = typ_to_ltyp t2 in
            let ltyp_s = (ltyp_to_str ltyp1) ^ (ltyp_to_str ltyp2) in
            let dict_name = "dict" ^ ltyp_s in
            L.pointer_type (if Hashtbl.mem dict_types dict_name then
                Hashtbl.find dict_types dict_name
            else (
                let dict_t = L.named_struct_type context dict_name in
                L.struct_set_body dict_t [|(L.pointer_type ltyp1); (L.pointer_type ltyp2); (L.pointer_type ht_t)|] false;
                Hashtbl.add dict_types dict_name dict_t;
                dict_t
            ))
        (*
        | A.UserTyp(ut)  -> let (_, at) = StringMap.find ut env.tsym in ltype_of_typ at
        *)
        | _        -> raise (Failure ("type" ^ not_impl))
(*
        | A.String ->
        | A.Regex  ->
        | A.List(t) ->
        | A.Dict(t1,t2)  ->
        | A.Fun(f,t)  ->
        | A.UserTypDef(u) ->
*)
    in

    (* Creating top-level function *)
    let main_t = L.function_type i32_t [||] in
    let main = L.define_function "main" main_t the_module in
    let builder = L.builder_at_end context (L.entry_block main) in

    (* start external functions *)
    let build_func (name, ret, args) var_arg =
        let t = if var_arg then L.var_arg_function_type ret args
        else L.function_type ret args
        in
        let func = L.declare_function name t the_module in
        func
    in
    let build_funcs map (def : string * L.lltype * L.lltype array) =
        let (name, _, _) = def in
        StringMap.add name (build_func def false) map
    in

    (* string stuff *)
    let printf_func   : L.llvalue = build_func ("printf", i32_t, [|string_t|]) true in
    let snprintf_func : L.llvalue = build_func ("snprintf", i32_t, [|string_t; i32_t; string_t|]) true in
    let strcpy_func   : L.llvalue = build_func ("strcpy", string_t, [|string_t; string_t|]) false in
    let strcat_func   : L.llvalue = build_func ("strcat", string_t, [|string_t; string_t|]) false in
    let strlen_func   : L.llvalue = build_func ("strlen", i64_t, [|string_t;|]) false in
    let str_format = L.build_global_stringptr "%s\n" "fmt" builder in
    let i32_format = L.build_global_stringptr "%d" "fmt" builder in
    let float_format = L.build_global_stringptr "%f" "fmt" builder in

    (* linked list functions *)
    let ll_create = "ll_create" in
    let ll_push   = "ll_push" in
    let ll_pop    = "ll_pop" in
    let ll_get    = "ll_get" in
    let ll_print  = "ll_print" in
    let ll_defs = [
        (ll_create, (L.pointer_type ll_node), [|string_t|]);
        (ll_push, (L.pointer_type ll_node), [|L.pointer_type ll_node; string_t|]);
        (ll_pop, (L.pointer_type ll_node), [|L.pointer_type ll_node|]);
        (ll_get, (string_t), [|L.pointer_type ll_node; i32_t|]);
        (ll_print, i32_t, [|L.pointer_type ll_node|]);
    ] in
    let ll_funcs = List.fold_left build_funcs StringMap.empty ll_defs in
    let ll_create_func = StringMap.find ll_create ll_funcs in
    let ll_push_func = StringMap.find ll_push ll_funcs in
    let ll_pop_func = StringMap.find ll_pop ll_funcs in
    let ll_get_func = StringMap.find ll_get ll_funcs in
    let ll_print_func = StringMap.find ll_print ll_funcs in

    (* hash table functions *)
    let ht_create  = "ht_create" in
    let ht_hash    = "ht_hash" in
    let ht_newpair = "ht_newpair" in
    let ht_set     = "ht_set" in
    let ht_get     = "ht_get" in
    let ht_print   = "ht_print" in
    let ht_defs = [
        (ht_create, (L.pointer_type ht_t), [|i32_t|]);
        (ht_hash, i32_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_newpair, (L.pointer_type ht_entry), [|string_t; string_t|]);
        (ht_get, string_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_set, (L.pointer_type ht_t), [|(L.pointer_type ht_t); string_t; string_t|]);
        (ht_print, i32_t, [|(L.pointer_type ht_t)|]);
    ] in
    let ht_funcs = List.fold_left build_funcs StringMap.empty ht_defs in
    let ht_create_func = StringMap.find ht_create ht_funcs in
    let ht_hash_func = StringMap.find ht_hash ht_funcs in
    let ht_newpair_func = StringMap.find ht_newpair ht_funcs in
    let ht_set_func = StringMap.find ht_set ht_funcs in
    let ht_get_func = StringMap.find ht_get ht_funcs in
    let ht_print_func = StringMap.find ht_print ht_funcs in
    (* end external functions *)

    (* start stdlib functions *)
    (* end stdlib functions *)

    (* variable name -> value table *)
    let var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
    (* func name -> locals table *)
    let func_locals_tbl : (string, (string * L.llvalue) list) Hashtbl.t = Hashtbl.create 10 in

    let rec expr parent_func builder (e : sx) =
        let make_addr (e : L.llvalue) (t : L.lltype) (malloc : bool) (builder : L.llbuilder) =
            let addr = if malloc then L.build_malloc t "" builder else L.build_alloca t "" builder
            in
            ignore(L.build_store e addr builder);
            addr
            (*
            match (L.classify_value e) with
                L.ValueKind.Instruction(_) -> e
                | _ ->
                    let addr = if malloc then L.build_malloc t "" builder else L.build_alloca t "" builder
                    in
                    ignore(L.build_store e addr builder);
                    addr
                    *)
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
            let addr = L.build_array_alloca i8_t (L.const_int i8_t len) "string" builder in
            let store_char i c =
                let i' = string_of_int i in
                let c' = Char.code c in
                let addr_i = L.build_in_bounds_gep addr [|L.const_int i8_t i|] (prefix ^ i') builder in
                ignore(L.build_store (L.const_int i8_t c') addr_i builder);
            in
            String.iteri store_char (s ^ "\x00");
            (builder, addr)
        | SListLit(t, l) ->
            let list_t = L.element_type (typ_to_ltyp (A.List(t))) in
            let ltyp = typ_to_ltyp t in

            let addr = L.build_malloc list_t "list" builder in
            let addr_ltyp = L.build_in_bounds_gep addr [|zero;zero|] "listltyp" builder in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in

            let null_t = L.build_malloc ltyp "null" builder in
            ignore(L.build_store (L.const_null ltyp) null_t builder);
            ignore(L.build_store null_t addr_ltyp builder);

            let l' = List.map (fun e -> snd e) l in
            let first = List.hd l' in
            let the_rest = List.tl l' in

            let (builder, first') = expr parent_func builder first in
            let data = make_addr first' ltyp true builder in
            let c_data = L.build_bitcast data string_t "cdata0" builder in
            let first_node = L.build_call ll_create_func [|c_data|] "node0" builder in
            ignore(L.build_store first_node addr_head builder);

            let add_node (builder, last_node, i) e =
                let i' = string_of_int i in
                let (builder, e') = expr parent_func builder e in
                let data = make_addr e' ltyp true builder in
                let c_data = L.build_bitcast data string_t ("cdata" ^ i') builder in
                let addr = L.build_call ll_push_func [|last_node; c_data|] ("node" ^ i') builder in
                (builder, addr, i+1)
            in
            let (builder, _, _) = List.fold_left add_node (builder, first_node, 1) the_rest in
            (builder, addr)
        | SDictLit(t1, t2, d) ->
            let dict_t = L.element_type (typ_to_ltyp (A.Dict(t1,t2))) in
            let ltyp1 = typ_to_ltyp t1 in
            let ltyp2 = typ_to_ltyp t2 in
            let addr = L.build_malloc dict_t "dict" builder in

            (* dummy values so we know the actual types of the table *)
            let null_t1 = L.build_malloc ltyp1 "nullt1" builder in
            let null_t2 = L.build_malloc ltyp2 "nullt2" builder in
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero; one|] "dictt2" builder in
            ignore(L.build_store (L.const_null ltyp1) null_t1 builder);
            ignore(L.build_store (L.const_null ltyp2) null_t2 builder);
            ignore(L.build_store null_t1 addr_t1 builder);
            ignore(L.build_store null_t2 addr_t2 builder);

            (* create dict *)
            (* dict size = prime num ~= 1.3 * number of elements *)
            let ht =  L.build_call ht_create_func [|L.const_int i32_t (List.length d)|] "tbl" builder in
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
                ignore(L.build_call ht_set_func [|ht;c_k_data;c_v_data|] "" builder);
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
            let err = "internal error" in
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
                    | _ -> raise (Failure err)
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
                    | _ -> raise (Failure err)
                ) e1' e2' "out" builder
                | A.Bool -> (match o with
                      A.And -> L.build_and
                    | A.Or  -> L.build_or
                    | _ -> raise (Failure err)
                ) e1' e2' "out" builder
                | A.String -> (match o with
                    A.Concat ->
                        let len1 = L.build_call strlen_func [|e1'|] "e1len" builder in
                        let len2 = L.build_call strlen_func [|e2'|] "e2len" builder in
                        let len = L.build_add len1 len2 "len" builder in
                        let out_addr = L.build_array_alloca i8_t len "out" builder in
                        let out_addr = L.build_call strcpy_func [|out_addr; e1'|] "cpy" builder in
                        let out_addr = L.build_call strcat_func [|out_addr; e2'|] "cat" builder in
                        out_addr
                    | _ -> raise (Failure err)
                )
                (*
                | A.List -> (match o with
                    A.Concat ->
                    | _ -> raise (Failure err)
                )
                *)
                | _ -> raise (Failure err)
            )
            in
            (builder, out)
        | SFunCall("lget", [(_, l); (_,n)]) ->
            let (builder, addr) = expr parent_func builder l in
            let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" builder in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in
            let ltyp_ptr = L.build_load addr_t "t*" builder in
            let ltyp = L.type_of (L.build_load ltyp_ptr "t" builder) in

            let head_node = L.build_load addr_head "headnode" builder in
            let (builder, n') = expr parent_func builder n in
            let c_data = L.build_call ll_get_func [|head_node;n'|] "cdata" builder in
            let data = L.build_bitcast c_data (L.type_of ltyp_ptr) "data" builder in
            let data_load = L.build_load data "dataload" builder in
            (builder, data_load)
        | SFunCall("dget", [(_, dict); (_,k)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ltyp1_ptr = L.build_load addr_t1 "t1*" builder in
            let ltyp2_ptr = L.build_load addr_t2 "t2*" builder in
            let ltyp1 = L.type_of (L.build_load ltyp1_ptr "t1" builder) in
            let ltyp2 = L.type_of (L.build_load ltyp2_ptr "t2" builder) in
            let (builder, k') = expr parent_func builder k in
            let k_data = make_addr k' ltyp1 false builder in
            let c_k_data = L.build_bitcast k_data string_t "ckdata" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let c_v_data = L.build_call ht_get_func [|ht;c_k_data|] "cvdata" builder in
            let v_data = L.build_bitcast c_v_data (L.type_of ltyp2_ptr) "vdata" builder in
            let v_data_load = L.build_load v_data "vdataload" builder in
            (builder, v_data_load)
        | SFunCall("dset", [(_, dict); (_,k); (_,v)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ltyp1 = L.type_of (L.build_load (L.build_load addr_t1 "t1*" builder) "t1" builder) in
            let ltyp2 = L.type_of (L.build_load (L.build_load addr_t2 "t2*" builder) "t2" builder) in
            let (builder, k') = expr parent_func builder k in
            let (builder, v') = expr parent_func builder v in
            let k_data = make_addr k' ltyp1 false builder in
            let v_data = make_addr v' ltyp2 false builder in
            let c_k_data = L.build_bitcast k_data string_t "ckdata" builder in
            let c_v_data = L.build_bitcast v_data string_t "cvdata" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let ht' = L.build_call ht_set_func [|ht;c_k_data;c_v_data|] "ht_" builder in
            ignore(L.build_store ht' addr_ht builder);
            (builder, addr)
        | SFunCall("sprint", [(typlist,e)]) ->
            let (builder, e') = expr parent_func builder e in
            let out = L.build_call printf_func [|str_format;e'|] "printf" builder in
            (builder, out)
        | SFunCall("lprint", [(_,e)]) ->
            let (builder, addr) = expr parent_func builder e in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in
            let head_node = L.build_load addr_head "headnode" builder in
            let out = L.build_call ll_print_func [|head_node|] "printlist" builder in
            (builder, out)
        | SFunCall("dprint", [(_,e)]) ->
            let (builder, addr) = expr parent_func builder e in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let out = L.build_call ht_print_func [|ht|] "printdict" builder in
            (builder, out)
        | SFunCall("tostring", [(_,e)]) ->
            let (builder, e') = expr parent_func builder e in
            let to_string fmt =
                let len = L.build_call snprintf_func [|L.const_null string_t;zero;fmt;e'|] "" builder
                in
                let len = L.build_add len one "" builder in
                let addr = L.build_array_alloca i8_t len "tempstr" builder in
                ignore(L.build_call snprintf_func [|addr;len;fmt;e'|] "" builder);
                addr
            in
            let t = L.type_of e' in
            let str = if t = i1_t then to_string i32_format
                else if t = i32_t then to_string i32_format
                else if t = float_t then to_string float_format
                else raise (Failure ((L.string_of_lltype t) ^ not_impl))
            in
            (builder, str)
        | SAssign(v, (_,e)) ->
            let (builder, e') = expr parent_func builder e in
            Hashtbl.add var_tbl v e';
            (builder, e')
        | SId(v) ->
            let func = L.value_name parent_func in
            let e' = try Hashtbl.find var_tbl v
                with Not_found -> (
                    try
                        let locals = Hashtbl.find func_locals_tbl func in
                        let (_, out) = List.find (fun (n,_) -> n = v) locals in
                        L.build_load out "local" builder
                    with Not_found ->
                    (
                        let print_tbl k v =
                            print_string("KEY: " ^ k);
                            print_string("VALUES: ");
                            List.iter (fun (s,l) -> print_string(s ^ " " ^ (L.string_of_llvalue l) ^ ";")) v;
                            print_endline("");
                        in
                        Hashtbl.iter print_tbl func_locals_tbl;
                        raise (Failure ("ID NOT FOUND: " ^ v))
                    )
                )
            in
            (builder, e')
        | SIfElse(i) ->
            let (builder, cond) = expr parent_func builder (snd i.sicond) in
            let merge_bb = L.append_block context "merge" parent_func in
            let build_br_merge = L.build_br merge_bb in
            let out_addr = L.build_alloca (typ_to_ltyp i.sityp) "out" builder in

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
            let out = L.build_load out_addr "out_val" builder in
            (builder, out)
        | SWhile(w) ->
            let ltyp = typ_to_ltyp w.swtyp in
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
            let out = L.build_load out_addr "out_val" builder in
            (builder, out)
        | SFunLit(f) ->
            let f_name = "fun" ^ (string_of_int !fun_name_i) in
            let fun_name_i = !fun_name_i + 1 in
            let ltyp = typ_to_ltyp f.sftyp in
            let formals_arr = Array.of_list (List.map (fun (t,_) -> typ_to_ltyp t) f.sformals) in
            let func_typ = L.function_type ltyp formals_arr in
            let func = L.define_function f_name func_typ the_module in

            let function_builder = L.builder_at_end context (L.entry_block func) in

            let params = Array.to_list (L.params func) in
            let make_param func_locals p (t,n) =
                L.set_value_name n p;
                let addr = L.build_alloca (typ_to_ltyp t) "" function_builder in
                (n, addr)::func_locals
            in
            let func_locals = List.fold_left2 make_param [] params f.sformals in
            Hashtbl.add func_locals_tbl f_name func_locals;

            let (_, function_builder, function_out) = List.fold_left stmt (func, function_builder, zero) f.sfblock in
            ignore(L.build_ret function_out function_builder);
            (builder, func)
        | SFunCall(v, l) ->
            (* TODO: make calling funlit w/o variable name *)
            (* let (builder, func) = expr parent_func builder f *)
            let func = try Hashtbl.find var_tbl v
                with Not_found -> raise (Failure "FUNC NOT FOUND")
            in
            let make_actuals (builder, actuals) (_, e) =
                let (builder, e') = expr parent_func builder e in
                (builder, e'::actuals)
            in
            let (builder, actuals) = List.fold_left make_actuals (builder, []) l in
            let actuals_arr = Array.of_list (List.rev actuals) in
            let out = L.build_call func actuals_arr "funcall" builder in
            (builder, out)
        | _ -> raise (Failure ("expr" ^ not_impl))
        (*
        *)
        (*
        | SReLit(r) -> L.const_string context r
        | SNullExpr -> ()
        | SUnop(o, e) -> ()
        | SChildAcc(e, s) -> ()
        | SCast(t, e) -> ()
        | STypDefAssign(t, v, l) -> ()
        | SMatch(m) -> ()
        | SUTDId(v) -> ()
        | SExpr(e) -> ()
                    *)
    and stmt (func, builder, _) = function
        SExprStmt(e) ->
            let (builder', out) = expr func builder (snd e) in
            (func, builder', out)
        | _      -> raise (Failure ("stmt" ^ not_impl))
    in
    let (_, builder, _) = List.fold_left stmt (main, builder, zero) sast in
    ignore(L.build_ret (L.const_int i32_t 0) builder);
    the_module
