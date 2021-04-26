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
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate env sast =
    (* Create the LLVM compilation module into which
       we will generate code *)
    let context = L.global_context () in
    let the_module = L.create_module context "ConLangLang" in

    (* to store different types of lists/dicts *)
    let list_types = Hashtbl.create 10 in
    let dict_types = Hashtbl.create 10 in

    (* Get types from the context *)
    let i32_t         = L.i32_type    context
    in let i64_t      = L.i64_type    context
    in let i1_t       = L.i1_type     context
    in let i8_t       = L.i8_type     context
    in let float_t    = L.double_type context
    in let string_t   = L.pointer_type i8_t
    in let void_t     = L.void_type   context
    in

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
            let ltyp = typ_to_ltyp t in
            let ltyp_s = ltyp_to_str ltyp in
            let list_name = "list" ^ ltyp_s in
            if Hashtbl.mem list_types list_name then
                Hashtbl.find list_types list_name
            else (
                let list_t = L.named_struct_type context list_name in
                L.struct_set_body list_t [|(L.pointer_type ltyp); (L.pointer_type list_t)|] false;
                Hashtbl.add list_types list_name list_t;
                list_t
            )
        | A.Dict(t1,t2) ->
            let ltyp1 = typ_to_ltyp t1 in
            let ltyp2 = typ_to_ltyp t2 in
            let ltyp_s = (ltyp_to_str ltyp1) ^ (ltyp_to_str ltyp2) in
            let dict_name = "dict" ^ ltyp_s in
            if Hashtbl.mem dict_types dict_name then
                Hashtbl.find dict_types dict_name
            else (
                let dict_t = L.named_struct_type context dict_name in
                L.struct_set_body dict_t [|(L.pointer_type ltyp1); (L.pointer_type ltyp2); (L.pointer_type ht_t)|] false;
                Hashtbl.add dict_types dict_name dict_t;
                dict_t
            )
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
    let func_t = L.function_type i32_t [||] in
    let main = L.define_function "main" func_t the_module in
    let builder = L.builder_at_end context (L.entry_block main) in

    (* start external functions *)
    let build_func (name, ret, args) =
        let t = L.var_arg_function_type ret args in
        let func = L.declare_function name t the_module in
        func
    in
    let build_funcs map (def : string * L.lltype * L.lltype array) =
        let (name, _, _) = def in
        StringMap.add name (build_func def) map
    in

    let printf_func : L.llvalue = build_func ("printf", i32_t, [|string_t|]) in
    let str_format = L.build_global_stringptr "%s\n" "fmt" builder in

    (* hash table functions *)
    let ht_create  = "ht_create" in
    let ht_hash    = "ht_hash" in
    let ht_newpair = "ht_newpair" in
    let ht_set     = "ht_set" in
    let ht_get     = "ht_get" in
    let ht_print_table = "ht_print_table" in
    let ht_defs = [
        (ht_create, (L.pointer_type ht_t), [|i32_t|]);
        (ht_hash, i32_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_newpair, (L.pointer_type ht_entry), [|string_t; string_t|]);
        (ht_get, string_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_set, (L.pointer_type ht_t), [|(L.pointer_type ht_t); string_t; string_t|]);
        (ht_print_table, void_t, [|(L.pointer_type ht_t)|]);
    ] in
    let ht_funcs = List.fold_left build_funcs StringMap.empty ht_defs in
    let ht_create_func = StringMap.find ht_create ht_funcs in
    let ht_hash_func = StringMap.find ht_hash ht_funcs in
    let ht_newpair_func = StringMap.find ht_newpair ht_funcs in
    let ht_set_func = StringMap.find ht_set ht_funcs in
    let ht_get_func = StringMap.find ht_get ht_funcs in
    let ht_print_table_func = StringMap.find ht_print_table ht_funcs in
    (* end external functions *)

    (* start stdlib functions *)
    (* end stdlib functions *)

    (* variable table *)
    let var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in

    let rec expr builder (e : sx) =
        let make_addr_if_const e t malloc builder =
            match (L.classify_value e) with
                L.ValueKind.Instruction(_) -> e
                | _ ->
                    let addr = if malloc then L.build_malloc t "" builder else L.build_alloca t "" builder
                    in
                    ignore(L.build_store e addr builder);
                    addr
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
            let addr = L.build_array_alloca i8_t (L.const_int i8_t len) "s" builder in
            let store_char i c =
                let i' = string_of_int i in
                let c' = Char.code c in
                let addr_i = L.build_in_bounds_gep addr [|L.const_int i8_t i|] (prefix ^ i') builder in
                ignore(L.build_store (L.const_int i8_t c') addr_i builder);
            in
            String.iteri store_char (s ^ "\x00");
            (builder, addr)
        | SListLit(t, l) ->
            let list_t = typ_to_ltyp (A.List(t)) in
            let l' = List.map (fun e -> snd e) l in
            let prefix = "l" in
            let data = "d" in
            let add_node i e =
                let addr = L.build_malloc list_t (prefix ^ string_of_int i) builder in
                (addr, e)
            in
            let node_addrs = List.mapi add_node l' in
            let node_addrs_rev = List.rev node_addrs in
            let null_addr = L.build_malloc list_t (prefix ^ string_of_int (List.length node_addrs)) builder in
            ignore(L.build_store (L.const_null (list_t)) null_addr builder);
            let make_node (builder, next_node_addr) (node_addr, e) =
                (* list_t = { void *data; list_t *next; } *)
                let (builder, node_data) = expr builder e in
                let node_data_addr = match t with
                    A.Int | A.Float | A.Bool | A.String  ->
                        let addr = L.build_malloc (typ_to_ltyp t) "" builder in
                        ignore(L.build_store node_data addr builder);
                        addr
                    | A.List(_) | A.Dict(_) | _ ->
                        node_data
                in
                (* node -> data *)
                let node_data_gep = L.build_in_bounds_gep node_addr [| zero; zero |] "" builder in
                ignore(L.build_store node_data_addr node_data_gep builder);
                (* node -> next *)
                let node_next_gep = L.build_in_bounds_gep node_addr [| zero; one |] "" builder in
                ignore(L.build_store next_node_addr node_next_gep builder);
                (builder, node_addr)
            in
            let (builder, _) = List.fold_left make_node (builder, null_addr) node_addrs_rev in
            (builder, fst (List.hd node_addrs))
        | SDictLit(t1, t2, d) ->
            let dict_t = typ_to_ltyp (A.Dict(t1,t2)) in
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
                let (builder, k') = expr builder k in
                let (builder, v') = expr builder v in
                let k_addr = make_addr_if_const k' ltyp1 false builder in
                let v_addr = make_addr_if_const v' ltyp2 false builder in
                let c_k = L.build_bitcast k_addr string_t ("ck" ^ string_of_int i) builder in
                let c_v = L.build_bitcast v_addr string_t ("cv" ^ string_of_int i) builder in
                ignore(L.build_call ht_set_func [|ht;c_k;c_v|] "" builder);
                (builder, i+1)
            in
            let (builder, _) = List.fold_left add_pair (builder, 0) d' in
            (builder, addr)
        | SFunCall("dget", [(_, dict); (_,k)]) ->
            let (builder, addr) = expr builder dict in
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ltyp1 = L.type_of (L.build_load (L.build_load addr_t1 "t1*" builder) "t1" builder) in
            let ltyp2 = L.type_of (L.build_load (L.build_load addr_t2 "t2*" builder) "t2" builder) in
            let (builder, k') = expr builder k in
            let k_addr = make_addr_if_const k' ltyp1 false builder in
            let c_k = L.build_bitcast k_addr string_t "ck" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let c_v = L.build_call ht_get_func [|ht;c_k|] "cv" builder in
            let v_addr = L.build_bitcast c_v ltyp2 "v" builder in
            (builder, v_addr)
        | SFunCall("dset", [(_, dict); (_,k); (_,v)]) ->
            let (builder, addr) = expr builder dict in
            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" builder in
            let ltyp1 = L.type_of (L.build_load (L.build_load addr_t1 "t1*" builder) "t1" builder) in
            let ltyp2 = L.type_of (L.build_load (L.build_load addr_t2 "t2*" builder) "t2" builder) in
            let (builder, k') = expr builder k in
            let (builder, v') = expr builder v in
            let k_addr = make_addr_if_const k' ltyp1 false builder in
            let v_addr = make_addr_if_const v' ltyp2 false builder in
            let c_k = L.build_bitcast k_addr string_t "ck" builder in
            let c_v = L.build_bitcast v_addr string_t "cv" builder in
            let ht = L.build_load addr_ht "ht" builder in
            let ht' = L.build_call ht_set_func [|ht;c_k;c_v|] "ht_" builder in
            ignore(L.build_store ht' addr_ht builder);
            (builder, addr)
        | SFunCall("sprint", [(typlist,e)]) ->
            let (builder, e') = expr builder e in
            let out = L.build_call printf_func [|str_format;e'|] "printf" builder
            in
            (builder, out)
        | SAssign(v, (typlist,e)) ->
            let t = typ_to_ltyp (List.hd typlist) in
            let (builder, e') = expr builder e in
            let addr = make_addr_if_const e' t false builder in
            Hashtbl.add var_tbl v addr;
            (builder, addr)
        | SId(v) ->
            let addr = Hashtbl.find var_tbl v in
            (builder, addr)
        | SIfElse(i) ->
            let (builder, cond) = expr builder (snd i.sicond) in
            let merge_bb = L.append_block context "merge" main in
            let build_br_merge = L.build_br merge_bb in
            let out_addr = L.build_alloca (typ_to_ltyp i.sityp) "out" builder in

            let then_bb = L.append_block context "then" main in
            let (if_builder, ifout) = List.fold_left stmt (L.builder_at_end context then_bb, zero) i.sifblock in
            ignore(L.build_store ifout out_addr if_builder);
            ignore(build_br_merge if_builder);

            let else_bb = L.append_block context "else" main in
            let (else_builder, elseout) = List.fold_left stmt (L.builder_at_end context else_bb, zero) i.selseblock in
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

            let cond_bb = L.append_block context "while" main in
            ignore(L.build_br cond_bb builder);
            let (cond_builder, cond) = expr (L.builder_at_end context cond_bb) (snd w.swcond) in

            let body_bb = L.append_block context "while_body" main in
            let (body_builder, body_out) = List.fold_left stmt (L.builder_at_end context body_bb, zero) w.swblock in
            ignore(L.build_store body_out out_addr body_builder);
            ignore(L.build_br cond_bb body_builder);

            let merge_bb = L.append_block context "merge" main in
            ignore(L.build_cond_br cond body_bb merge_bb cond_builder);
            let builder = L.builder_at_end context merge_bb in
            let out = L.build_load out_addr "out_val" builder in
            (builder, out)
        | _ -> raise (Failure ("expr" ^ not_impl))
        (*
        *)
        (*
        | SReLit(r) -> L.const_string context r
        | SFunLit(f) -> ()
        | SNullExpr -> ()
        | SBinop(e1, o, e2) -> ()
        | SUnop(o, e) -> ()
        | SChildAcc(e, s) -> ()
        | SCast(t, e) -> ()
        | STypDefAssign(t, v, l) -> ()
        | SMatch(m) -> ()
        | SUTDId(v) -> ()
        | SExpr(e) -> ()
                    *)
    and stmt (builder, _) = function
        SExprStmt(e) ->
            let (builder', out) = expr builder (snd e) in
            (builder', out)
        | _      -> raise (Failure ("stmt" ^ not_impl))
    in
    let (builder, _) = List.fold_left stmt (builder, zero) sast in
    ignore(L.build_ret (L.const_int i32_t 0) builder);
    the_module
