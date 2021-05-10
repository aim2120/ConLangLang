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
    let fun_name_i : int ref = ref 0 in
    let ut_i : int ref = ref 0 in

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

    (* matches structs in lib/regex.c *)
    let re_guts = L.named_struct_type context "re_guts" in
    let regex_t = L.named_struct_type context "regex_t" in
    L.struct_set_body regex_t [|i32_t;i64_t;string_t;L.pointer_type re_guts|] false;
    let regmatch_t = L.named_struct_type context "regmatch_t" in
    L.struct_set_body regmatch_t [|i64_t;i64_t|] false;

    (* convenient numbers/strings *)
    let zero = L.const_int i32_t 0 in
    let one = L.const_int i32_t 1 in
    let two = L.const_int i32_t 2 in
    let max_int = L.const_int i32_t 2147483647 in
    let tru = L.const_int i1_t 1 in
    let fals = L.const_int i1_t 0 in
    let not_impl = " not implemented" in
    let internal_err = "internal error" in

    (* Return the LLVM type for a MicroC type *)
    let str_of_ltyp ltyp =
        let janky_str t =
            let s = L.string_of_lltype ltyp in
            let r = Str.regexp " " in
            Str.global_replace r "_" s
        in
        let s = (match L.classify_type ltyp with
            L.TypeKind.Struct -> (match L.struct_name ltyp with
                Some n -> n
                | None -> janky_str ltyp
            )
            | _ -> janky_str ltyp
        ) in
        let r = Str.regexp "\"" in
        Str.global_replace r "" s
    in
    let rec ltyp_of_typ = function
          A.Int    -> i32_t
        | A.Bool   -> i1_t
        | A.Float  -> float_t
        | A.String -> string_t
        | A.Regex  -> L.pointer_type regex_t
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
    in

    (* Creating top-level function *)
    let main_t = L.function_type i32_t [||] in
    let main = L.define_function "main" main_t the_module in
    let builder = L.builder_at_end context (L.entry_block main) in

    (* variable name -> (value, parent_function) table *)
    let var_tbl : (string, (L.llvalue * L.llvalue)) Hashtbl.t = Hashtbl.create 10 in
    (* func name -> locals table *)
    let func_params_tbl : (string, (string, L.llvalue) Hashtbl.t) Hashtbl.t = Hashtbl.create 10 in
    let func_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in

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

    let exit_func : L.llvalue = declare_func ("exit", void_t, [|i32_t|]) false in

    (* malloc manager functions *)
    let init_malloc_addr_func : L.llvalue = declare_func ("init_malloc_addr", void_t, [||]) false in
    let add_malloc_addr_func : L.llvalue = declare_func ("add_malloc_addr", void_t, [|L.pointer_type i8_t|]) false in
    let free_malloc_addrs_func : L.llvalue = declare_func ("free_malloc_addrs", void_t, [||]) false in

    ignore(L.build_call init_malloc_addr_func [||] "" builder);

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

    (*
     * USE THIS TO DEBUG
    let (_, s') = expr parent_func builder (SStrLit(s)) in
    ignore(L.build_call printf_func [|debug_str_format;s'|] "printf" builder);
    *)

    (* start stdlib functions *)
    (* linked list functions *)
    let ll_create = "ll_create" in
    let ll_add    = "ll_add" in
    let ll_append = "ll_append" in
    let ll_next   = "ll_next" in
    let ll_mem    = "ll_mem" in
    let ll_get    = "ll_get" in
    let ll_remove = "ll_remove" in
    let ll_print  = "ll_print" in
    let ll_size   = "ll_size" in
    let ll_del    = "ll_del" in
    let ll_defs = [
        (ll_create, (L.pointer_type ll_node), [|string_t|]);
        (ll_add, (L.pointer_type ll_node), [|L.pointer_type ll_node;string_t;i32_t|]);
        (ll_append, (L.pointer_type ll_node), [|L.pointer_type ll_node;L.pointer_type ll_node|]);
        (ll_next, (L.pointer_type ll_node), [|L.pointer_type ll_node|]);
        (ll_mem, (i32_t), [|L.pointer_type ll_node;string_t;i1_t|]);
        (ll_get, (string_t), [|L.pointer_type ll_node; i32_t|]);
        (ll_remove, (L.pointer_type ll_node), [|L.pointer_type ll_node;i32_t|]);
        (ll_print, i32_t, [|L.pointer_type ll_node|]);
        (ll_size, i32_t, [|L.pointer_type ll_node|]);
        (ll_del, void_t, [|L.pointer_type ll_node|]);
    ] in
    let ll_funcs = List.fold_left declare_funcs StringMap.empty ll_defs in
    let ll_create_func = StringMap.find ll_create ll_funcs in
    let ll_add_func = StringMap.find ll_add ll_funcs in
    let ll_append_func = StringMap.find ll_append ll_funcs in
    let ll_next_func = StringMap.find ll_next ll_funcs in
    let ll_mem_func = StringMap.find ll_mem ll_funcs in
    let ll_get_func = StringMap.find ll_get ll_funcs in
    let ll_remove_func = StringMap.find ll_remove ll_funcs in
    let ll_print_func = StringMap.find ll_print ll_funcs in
    let ll_size_func = StringMap.find ll_size ll_funcs in
    let ll_del_func = StringMap.find ll_del ll_funcs in

    (* hash table functions *)
    let ht_create    = "ht_create" in
    let ht_hash      = "ht_hash" in
    let ht_newpair   = "ht_newpair" in
    let ht_add       = "ht_add" in
    let ht_mem       = "ht_mem" in
    let ht_get       = "ht_get" in
    let ht_remove    = "ht_remove" in
    let ht_print     = "ht_print" in
    let ht_size      = "ht_size" in
    let ht_keys      = "ht_keys" in
    let ht_keys_list = "ht_keys_list" in
    let ht_defs = [
        (ht_create, (L.pointer_type ht_t), [|i32_t; i1_t|]);
        (ht_hash, i32_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_newpair, (L.pointer_type ht_entry), [|string_t; string_t|]);
        (ht_mem, i1_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_get, string_t, [|(L.pointer_type ht_t); string_t|]);
        (ht_remove, (L.pointer_type ht_t), [|(L.pointer_type ht_t); string_t|]);
        (ht_add, (L.pointer_type ht_t), [|(L.pointer_type ht_t); string_t; string_t|]);
        (ht_print, i32_t, [|(L.pointer_type ht_t)|]);
        (ht_size, i32_t, [|L.pointer_type ht_t|]);
        (ht_keys, (L.pointer_type string_t), [|L.pointer_type ht_t|]);
        (ht_keys_list, (L.pointer_type ll_node), [|L.pointer_type ht_t|]);
    ] in
    let ht_funcs = List.fold_left declare_funcs StringMap.empty ht_defs in
    let ht_create_func = StringMap.find ht_create ht_funcs in
    let ht_hash_func = StringMap.find ht_hash ht_funcs in
    let ht_newpair_func = StringMap.find ht_newpair ht_funcs in
    let ht_add_func = StringMap.find ht_add ht_funcs in
    let ht_mem_func = StringMap.find ht_mem ht_funcs in
    let ht_get_func = StringMap.find ht_get ht_funcs in
    let ht_remove_func = StringMap.find ht_remove ht_funcs in
    let ht_print_func = StringMap.find ht_print ht_funcs in
    let ht_size_func = StringMap.find ht_size ht_funcs in
    let ht_keys_func = StringMap.find ht_keys ht_funcs in
    let ht_keys_list_func = StringMap.find ht_keys_list ht_funcs in

    (* regex functions *)
    let re_create_func : L.llvalue = declare_func ("re_create", L.pointer_type regex_t, [|string_t|]) false in
    let re_match_func : L.llvalue = declare_func ("re_match", i1_t, [|L.pointer_type regex_t;string_t|]) false in
    let re_sub_func : L.llvalue = declare_func ("re_sub", string_t, [|L.pointer_type regex_t;string_t;string_t;i32_t|]) false in

    (* end stdlib functions *)
    (* end external functions *)

    let rec expr parent_func builder (e : sx) =
        let assc_typ_of_typlist typlist =
            let t = List.hd typlist in
            (match t with
                A.UserTyp(ut) -> snd (StringMap.find ut env.tsym)
                | _ -> t)
        in

        let make_safe_malloc malloc_call (t : L.lltype) (builder : L.llbuilder) (func : L.llvalue) =
            let exit_bb = L.append_block context "endprog" func in
            let exit_builder = L.builder_at_end context exit_bb in
            ignore(L.build_call exit_func [|one|] "" exit_builder);
            ignore(L.build_unreachable exit_builder);

            let cont_bb = L.append_block context "contprog" func in

            let addr = malloc_call builder in
            let cond = L.build_icmp L.Icmp.Eq addr (L.const_null (L.pointer_type (t))) "mallocnull" builder in
            ignore(L.build_cond_br cond exit_bb cont_bb builder);

            let builder = L.builder_at_end context cont_bb in
            let c_addr = L.build_bitcast addr (L.pointer_type i8_t) "caddr" builder in
            ignore(L.build_call add_malloc_addr_func [|c_addr|] "" builder);

            (builder, addr)
        in

        let make_addr (e : L.llvalue) (t : L.lltype) (malloc : bool) (builder : L.llbuilder) (func : L.llvalue) =
            let (builder, addr) = if malloc then (
                make_safe_malloc (L.build_malloc t "") t builder func
            ) else (builder, (L.build_alloca t "" builder))
            in
            (if L.is_null addr then raise (Failure "malloc failed") else ());
            ignore(L.build_store e addr builder);
            (builder, addr)
        in

        let make_func f_name formals ret_typ =
            let formals_arr = Array.of_list (List.map (fun (t,_) -> t) formals) in
            let func_typ = L.function_type ret_typ formals_arr in
            let func = L.define_function f_name func_typ the_module in

            let function_builder = L.builder_at_end context (L.entry_block func) in
            let params = Array.to_list (L.params func) in
            let func_locals = Hashtbl.create (List.length formals) in
            let make_param p (t,n) =
                L.set_value_name n p;
                let addr = L.build_alloca t "" function_builder in
                ignore(L.build_store p addr function_builder);
                Hashtbl.add func_locals n addr;
            in
            List.iter2 make_param params formals;
            Hashtbl.add func_params_tbl f_name func_locals;
            (func, function_builder)
        in

        let add_parent_vars_to_formals formals =
            let last : string ref = ref "" in
            let get_parent_vars k (v, v_parent_func) a =
                (* only get the most recently added k *)
                if k = !last then a
                (* don't add vars that share name with a formal param *)
                else (match (List.find_opt (fun (_,n) -> k = n) formals) with
                    Some (_,_) -> a
                    | None -> (match L.classify_value v with
                        L.ValueKind.Instruction(_) ->
                            if v_parent_func = parent_func then (
                                let t = L.type_of v in
                                last := k;
                                (t,k)::a
                            )
                            else a
                        | _ -> a
                    )
                )
            in
            let parent_vars = Hashtbl.fold get_parent_vars var_tbl [] in
            formals @ parent_vars
        in

        let add_parent_vars_to_actuals params actuals =
            let actuals_len = List.length actuals in
            let get_parent_vars (parent_vars, i) param =
                (* skip the vars that are part of normal function declaration *)
                if i < actuals_len then (parent_vars, i+1)
                else (
                    let (v, _) = Hashtbl.find var_tbl (L.value_name param) in
                    (v::parent_vars, i+1)
                )
            in
            let (parent_vars, _) = Array.fold_left get_parent_vars ([], 0) params in
            actuals @ (List.rev parent_vars)
        in

        let init_params func f_name builder =
            let init_param i param =
                let param_name = L.value_name param in
                let out = try Hashtbl.find (Hashtbl.find func_params_tbl f_name) param_name
                    with Not_found -> raise (Failure ("Param not found: " ^ param_name))
                in
                let out_load = L.build_load out "param" builder in
                Hashtbl.add var_tbl param_name (out_load, func);
            in
            Array.iteri init_param (L.params func);
        in

        let cleanup_func_vars func =
            let find_funlit_vars k (v, v_parent_func) to_remove =
                if v_parent_func = func then
                    k::to_remove
                else to_remove
            in
            let to_remove = Hashtbl.fold find_funlit_vars var_tbl [] in
            List.iter (fun k -> Hashtbl.remove var_tbl k) to_remove;
        in

        let make_ladd_func (f_name : string) (addr : L.llvalue) (e' : L.llvalue) =
            let addr_typ = L.type_of addr in
            let ltyp = L.type_of e' in
            let (func, function_builder) = make_func f_name [(addr_typ,"#l");(ltyp,"#e");i32_t,"#n"] addr_typ in

            init_params func f_name function_builder;

            let (function_builder, addr) = expr func function_builder (SId("#l")) in
            let (function_builder, e') = expr func function_builder (SId("#e")) in
            let (function_builder, n') = expr func function_builder (SId("#n")) in

            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
            let head_node = L.build_load addr_head "headnode" function_builder in

            let (function_builder, data) = make_addr e' ltyp true function_builder func in
            let c_data = L.build_bitcast data string_t "cdata" function_builder in

            let head_node' = L.build_call ll_add_func [|head_node;c_data;n'|] "" function_builder in
            ignore(L.build_store head_node' addr_head function_builder);
            ignore(L.build_ret addr function_builder);

            cleanup_func_vars func;

            func
        in

        let make_dadd_func (f_name : string) (addr : L.llvalue) (k' : L.llvalue) (v' : L.llvalue) =
            let addr_typ = L.type_of addr in
            let k_typ = L.type_of k' in
            let v_typ = L.type_of v' in
            let (func, function_builder) = make_func f_name [(addr_typ,"#d");(k_typ,"#k");(v_typ,"#v")] addr_typ in

            init_params func f_name function_builder;

            (* building dadd function *)
            let (function_builder, addr) = expr func function_builder (SId("#d")) in
            let (function_builder, k')   = expr func function_builder (SId("#k")) in
            let (function_builder, v')   = expr func function_builder (SId("#v")) in

            let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
            let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" function_builder in
            let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
            let ltyp1 = L.element_type (L.element_type (L.type_of addr_t1)) in
            let ltyp2 = L.element_type (L.element_type (L.type_of addr_t2)) in

            let (function_builder, k_data) = make_addr k' ltyp1 false function_builder func in
            let (function_builder, v_data) = make_addr v' ltyp2 false function_builder func in
            let c_k_data = L.build_bitcast k_data string_t "ckdata" function_builder in
            let c_v_data = L.build_bitcast v_data string_t "cvdata" function_builder in
            let ht = L.build_load addr_ht "ht" function_builder in
            let ht' = L.build_call ht_add_func [|ht;c_k_data;c_v_data|] "ht_" function_builder in
            (if L.is_null ht' then raise (Failure "malloc failed") else ());
            ignore(L.build_store ht' addr_ht function_builder);
            ignore(L.build_ret addr function_builder);

            cleanup_func_vars func;

            func
        in

        let make_lget_func (f_name : string) (addr : L.llvalue) (builder: L.llbuilder) =
            let addr_typ = L.type_of addr in
            let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" builder in
            let t = L.element_type (L.element_type (L.type_of addr_t)) in
            let (func, function_builder) = make_func f_name [(addr_typ,"#l");(i32_t,"#n")] t in

            init_params func f_name function_builder;

            let (function_builder, addr) = expr func function_builder (SId("#l")) in
            let (function_builder, n') = expr func function_builder (SId("#n")) in

            let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" function_builder in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
            let ltyp_ptr = L.build_load addr_t "t*" function_builder in

            let head_node = L.build_load addr_head "headnode" function_builder in
            let c_data = L.build_call ll_get_func [|head_node;n'|] "cdata" function_builder in
            let data = L.build_bitcast c_data (L.type_of ltyp_ptr) "data" function_builder in
            let data_load = L.build_load data "dataload" function_builder in
            ignore(L.build_ret data_load function_builder);

            cleanup_func_vars func;

            func
        in

        let make_lfold_func (f_name : string) (arg_func : L.llvalue) (a' : L.llvalue) (addr : L.llvalue) =

            let arg_func_typ = L.type_of arg_func in
            let accum_typ = L.type_of a' in
            let addr_typ = L.type_of addr in

            let formals = [(arg_func_typ,"#f");(accum_typ,"#a");(addr_typ,"#l")] in
            let (func, function_builder) = make_func f_name (add_parent_vars_to_formals formals) accum_typ in

            init_params func f_name function_builder;

            let arg_func_params = L.params arg_func in

            let (function_builder, arg_func) = expr func function_builder (SId("#f")) in
            let (function_builder, a') = expr func function_builder (SId("#a")) in
            let (function_builder, addr) = expr func function_builder (SId("#l")) in

            let i_addr = L.build_alloca i32_t "iaddr" function_builder in
            ignore(L.build_store zero i_addr function_builder);
            let accum_t = L.type_of a' in
            let (function_builder, accum_addr) = make_safe_malloc (L.build_malloc accum_t "accum") accum_t function_builder func in
            ignore(L.build_store a' accum_addr function_builder);

            let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listltyp" function_builder in
            let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
            let head_node = L.build_load addr_head "headnode" function_builder in
            let curr_node_t = L.pointer_type ll_node in
            let (function_builder, curr_node) = make_safe_malloc (L.build_malloc curr_node_t "currnode") curr_node_t function_builder func in
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

            let a' = L.build_load accum_addr "accumload" body_builder in

            let actuals = add_parent_vars_to_actuals arg_func_params [a';data_load] in
            let actuals_arr = Array.of_list actuals in
            let a' = L.build_call arg_func actuals_arr "accumresult" body_builder in

            ignore(L.build_store a' accum_addr body_builder);

            let i = L.build_add i one "i" body_builder in
            ignore(L.build_store i i_addr body_builder);

            let next_node = L.build_call ll_next_func [|node|] "nextnode" body_builder in
            ignore(L.build_store next_node curr_node body_builder);

            let merge_bb = L.append_block context "merge" func in

            ignore(L.build_br cond_bb function_builder);
            ignore(L.build_br cond_bb body_builder);
            ignore(L.build_cond_br cond body_bb merge_bb cond_builder);

            let function_builder = L.builder_at_end context merge_bb in
            let accum_final = L.build_load accum_addr "lfoldaccum" function_builder in

            ignore(L.build_ret accum_final function_builder);

            cleanup_func_vars func;

            func
        in

        let make_dfold_func (f_name : string) (arg_func : L.llvalue) (a' : L.llvalue) (addr : L.llvalue) =
            let arg_func_typ = L.type_of arg_func in
            let accum_typ = L.type_of a' in
            let addr_typ = L.type_of addr in

            let formals = [(arg_func_typ,"#f");(accum_typ,"#a");(addr_typ,"#d")] in
            let (func, function_builder) = make_func f_name (add_parent_vars_to_formals formals) accum_typ in

            init_params func f_name function_builder;

            let arg_func_params = L.params arg_func in

            let (function_builder, arg_func) = expr func function_builder (SId("#f")) in
            let (function_builder, a') = expr func function_builder (SId("#a")) in
            let (function_builder, addr) = expr func function_builder (SId("#d")) in

            let i_addr = L.build_alloca i32_t "iaddr" function_builder in
            ignore(L.build_store zero i_addr function_builder);
            let accum_addr = L.build_malloc (L.type_of a') "accum" function_builder in
            ignore(L.build_store a' accum_addr function_builder);

            let dictt1_addr = L.build_in_bounds_gep addr [|zero;zero|] "dictt1addr" function_builder in
            let dictt2_addr = L.build_in_bounds_gep addr [|zero;one|] "dictt2addr" function_builder in
            let t1_ptr = L.element_type (L.type_of dictt1_addr) in
            let t2_ptr = L.element_type (L.type_of dictt2_addr) in
            let dictht_addr = L.build_in_bounds_gep addr [|zero;two|] "dicthtaddr" function_builder in
            let ht = L.build_load dictht_addr "ht" function_builder in
            let keys = L.build_call ht_keys_func [|ht|] "keys" function_builder in
            let size = L.build_call ht_size_func [|ht|] "size" function_builder in

            let cond_bb = L.append_block context "cond" func in
            let cond_builder = L.builder_at_end context cond_bb in
            let i = L.build_load i_addr "i" cond_builder in
            let cond = L.build_icmp L.Icmp.Slt i size "lessthan" cond_builder in

            let body_bb = L.append_block context "foldbody" func in
            let body_builder = L.builder_at_end context body_bb in

            let curr_key_gep = L.build_in_bounds_gep keys [|i|] "currkeygep" body_builder in
            let curr_key_c = L.build_load curr_key_gep "currkeyc" body_builder in
            let curr_value_c = L.build_call ht_get_func [|ht;curr_key_c|] "currvaluec" body_builder in
            let curr_key_addr = L.build_bitcast curr_key_c t1_ptr "currkeyaddr" body_builder in
            let curr_value_addr = L.build_bitcast curr_value_c t2_ptr "currvalueaddr" body_builder in
            let curr_key = L.build_load curr_key_addr "currkey" body_builder in
            let curr_value = L.build_load curr_value_addr "currval" body_builder in

            let a' = L.build_load accum_addr "accumload" body_builder in
            let actuals = add_parent_vars_to_actuals arg_func_params [a';curr_key;curr_value] in
            let actuals_arr = Array.of_list actuals in
            let a' = L.build_call arg_func actuals_arr "accumresult" body_builder in
            ignore(L.build_store a' accum_addr body_builder);

            let i = L.build_add i one "i" body_builder in
            ignore(L.build_store i i_addr body_builder);

            let merge_bb = L.append_block context "merge" func in

            ignore(L.build_br cond_bb function_builder);
            ignore(L.build_br cond_bb body_builder);
            ignore(L.build_cond_br cond body_bb merge_bb cond_builder);

            let function_builder = L.builder_at_end context merge_bb in
            let accum_final = L.build_load accum_addr "dfoldaccum" function_builder in

            ignore(L.build_ret accum_final function_builder);

            cleanup_func_vars func;

            func
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
            let (builder, addr) = make_safe_malloc (L.build_array_malloc i8_t (L.const_int i32_t len) "string") i8_t builder parent_func in
            (if L.is_null addr then raise (Failure "malloc failed") else ());
            let store_char i c =
                let i' = string_of_int i in
                let c' = Char.code c in
                let addr_i = L.build_in_bounds_gep addr [|L.const_int i32_t i|] (prefix ^ i') builder in
                ignore(L.build_store (L.const_int i8_t c') addr_i builder);
            in
            String.iteri store_char (s ^ "\x00");
            (builder, addr)

        | SReLit(r) ->
            let (builder, addr) = expr parent_func builder (SStrLit(r)) in
            let regex = L.build_call re_create_func [|addr|] "regex" builder in
            (builder,regex)
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

            let l' = List.rev (List.map (fun e -> snd e) l) in
            let (builder, addr) = (match l' with

                last::the_rest -> (
                    let (builder, last') = expr parent_func builder last in

                    let ladd_f_name = "ladd" ^ (str_of_ltyp (L.type_of addr)) in
                    let ladd_func = (try Hashtbl.find func_tbl ladd_f_name
                        with Not_found -> (
                            let func = make_ladd_func ladd_f_name addr last' in
                            Hashtbl.add func_tbl ladd_f_name func;
                            func
                        )
                    ) in
                    let addr = L.build_call ladd_func [|addr;last';zero|] "listlitadd" builder in

                    let add_node (builder, addr) e =
                        let (builder, e') = expr parent_func builder e in
                        let addr = L.build_call ladd_func [|addr;e';zero|] "listlitadd" builder in
                        (builder, addr)
                    in
                    List.fold_left add_node (builder, addr) the_rest
                )
                | [] -> (
                    ignore(L.build_store (L.const_null (L.pointer_type ll_node)) addr_head builder);
                    (builder, addr)
                )
            ) in
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
                let (builder, k_data) = make_addr k' ltyp1 true builder parent_func in
                let (builder, v_data) = make_addr v' ltyp2 true builder parent_func in
                let c_k_data = L.build_bitcast k_data string_t ("ckdata" ^ i') builder in
                let c_v_data = L.build_bitcast v_data string_t ("cvdata" ^ i') builder in
                let ht = L.build_call ht_add_func [|ht;c_k_data;c_v_data|] "" builder in
                (if L.is_null ht then raise (Failure "malloc failed") else ());
                (builder, i+1)
            in
            let (builder, _) = List.fold_left add_pair (builder, 0) d' in
            (builder, addr)

        | STypComp((typlist,_), t) ->
            let out_addr = L.build_alloca i1_t "typcompout" builder in
            ignore(L.build_store fals out_addr builder);

            let merge_bb = L.append_block context "merge" parent_func in

            let true_bb = L.append_block context "typcomptrue" parent_func in
            let true_builder = L.builder_at_end context true_bb in
            ignore(L.build_store tru out_addr true_builder);
            ignore(L.build_br merge_bb true_builder);

            let rhs_name = match t with A.UserTyp(n) -> n | _ -> raise (Failure internal_err) in
            let rhs_val = match (L.lookup_global rhs_name the_module) with Some g -> g | None -> raise (Failure internal_err) in
            let make_typ_comp (blocks, i) typ = (
                match typ with
                    A.UserTyp(lhs_name) -> (
                        let i' = string_of_int i in
                        let lhs_val = match (L.lookup_global lhs_name the_module) with Some g -> g | None -> raise (Failure internal_err) in
                        let cond_bb = L.append_block context ("typcomp" ^ i') parent_func in
                        let cond_builder = L.builder_at_end context cond_bb in
                        let comp_result = L.build_icmp L.Icmp.Eq rhs_val lhs_val ("typcompresult" ^ i') cond_builder in
                        ignore(L.build_cond_br comp_result true_bb (List.hd blocks) cond_builder);
                        (cond_bb::blocks, i+1)
                    )
                    (* do nothing if not a usertyp *)
                    | _ -> (blocks, i)
                )
            in
            let (blocks, _) = List.fold_left make_typ_comp ([merge_bb], 0) typlist in

            ignore(L.build_br (List.hd blocks) builder);

            let builder = L.builder_at_end context merge_bb in
            let out_load = L.build_load out_addr "typcompout" builder in
            (builder, out_load)

        | SBinop((typlist,e1), o, (_,e2)) ->
            let t = assc_typ_of_typlist typlist in
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

                | A.List(t) -> (match o with
                    A.Concat ->
                        let (builder, addr) = expr parent_func builder (SListLit(t,[])) in
                        let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" builder in
                        let e1_addr_head = L.build_in_bounds_gep e1' [|zero;one|] "e1listhead" builder in
                        let e2_addr_head = L.build_in_bounds_gep e2' [|zero;one|] "e2listhead" builder in
                        let e1_head_node = L.build_load e1_addr_head "e1headnode" builder in
                        let e2_head_node = L.build_load e2_addr_head "e2headnode" builder in
                        let head_node = L.build_call ll_append_func [|e1_head_node;e2_head_node|] "concatheadnode" builder in
                        ignore(L.build_store head_node addr_head builder);
                        addr

                    | _ -> raise (Failure internal_err)
                )
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

        | SFunCall((t,SId("ladd")), [(t_l,l); (t_e,e)]) ->
            expr parent_func builder (SFunCall((t,SId("ladd")), [(t_l,l); (t_e,e); ([A.Int],SIntLit(0))]))

        | SFunCall((_,SId("ladd")), [(_,l); (_,e); (_,n)]) ->
            let (builder, addr) = expr parent_func builder l in
            let (builder, e') = expr parent_func builder e in
            let (builder, n') = expr parent_func builder n in
            let f_name = "ladd" ^ (str_of_ltyp (L.type_of addr)) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let func = make_ladd_func f_name addr e' in
                    Hashtbl.add func_tbl f_name func;
                    func
                )
            ) in
            let addr' = L.build_call func [|addr;e';n'|] "ladd" builder in
            (builder, addr')

        | SFunCall((_,SId("dadd")), [(_, dict); (_,k); (_,v)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k')   = expr parent_func builder k in
            let (builder, v')   = expr parent_func builder v in
            let f_name = "dadd" ^ (str_of_ltyp (L.type_of addr)) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let func = make_dadd_func f_name addr k' v' in
                    Hashtbl.add func_tbl f_name func;
                    func
                )
            ) in

            let addr = L.build_call func [|addr;k';v'|] "dadd" builder in
            (builder, addr)

        | SFunCall((_,SId("lmem")), [(_, l); (_,e)]) ->
            let (builder, addr) = expr parent_func builder l in
            let (builder, e') = expr parent_func builder e in
            let addr_typ = L.type_of addr in
            let f_name = "lmem" ^ (str_of_ltyp addr_typ) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" builder in
                    let t = L.element_type (L.element_type (L.type_of addr_t)) in
                    let (func, function_builder) = make_func f_name [(addr_typ,"#l");(t,"#e")] i32_t in

                    init_params func f_name function_builder;

                    let (function_builder, addr) = expr func function_builder (SId("#l")) in
                    let (function_builder, e') = expr func function_builder (SId("#e")) in

                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" function_builder in
                    let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
                    let ltyp_ptr = L.build_load addr_t "t*" function_builder in
                    let head_node = L.build_load addr_head "headnode" function_builder in

                    let (function_builder, data) = make_addr e' t false function_builder func in
                    let c_data = L.build_bitcast data string_t "cdata" function_builder in

                    let n = if t = string_t then
                        L.build_call ll_mem_func [|head_node;c_data;tru|] "cdata" function_builder
                    else
                        L.build_call ll_mem_func [|head_node;c_data;fals|] "cdata" function_builder
                    in
                    ignore(L.build_ret n function_builder);

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let n = L.build_call func [|addr;e'|] "lmem" builder in
            (builder, n)

        | SFunCall((_,SId("dmem")), [(_, dict); (_,k)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k') = expr parent_func builder k in
            let addr_typ = L.type_of addr in
            let f_name = "dmem" ^ (str_of_ltyp addr_typ) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let k_typ = L.type_of k' in
                    let (func, function_builder) = make_func f_name [(addr_typ,"#d");(k_typ,"#k")] i1_t in

                    init_params func f_name function_builder;

                    (* building dmem function *)
                    let (function_builder, addr) = expr func function_builder (SId("#d")) in
                    let (function_builder, k')   = expr func function_builder (SId("#k")) in

                    let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
                    let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
                    let ht = L.build_load addr_ht "ht" function_builder in
                    let ltyp1 = L.element_type (L.element_type (L.type_of addr_t1)) in

                    let (function_builder, k_data) = make_addr k' ltyp1 false function_builder func in
                    let c_k_data = L.build_bitcast k_data string_t "ckdata" function_builder in
                    let is_mem = L.build_call ht_mem_func [|ht;c_k_data|] "dmem" function_builder in
                    ignore(L.build_ret is_mem function_builder);

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let is_mem = L.build_call func [|addr;k'|] "dmem" builder in
            (builder, is_mem)



        | SFunCall((_,SId("lget")), [(_, l); (_,n)]) ->
            let (builder, addr) = expr parent_func builder l in
            let (builder, n') = expr parent_func builder n in
            let addr_type = L.type_of addr in
            let f_name = "lget" ^ (str_of_ltyp addr_type) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let func = make_lget_func f_name addr builder in
                    Hashtbl.add func_tbl f_name func;
                    func
                )
            ) in

            let data_load = L.build_call func [|addr;n'|] "lget" builder in
            (builder, data_load)

        | SFunCall((_,SId("dget")), [(_, dict); (_,k)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k') = expr parent_func builder k in
            let addr_typ = L.type_of addr in
            let f_name = "dget" ^ (str_of_ltyp addr_typ) in

            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let k_typ = L.type_of k' in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" builder in
                    let v_typ = L.element_type (L.element_type (L.type_of addr_t2)) in
                    let (func, function_builder) = make_func f_name [(addr_typ,"#d");(k_typ,"#k")] v_typ in

                    init_params func f_name function_builder;

                    (* building dget function *)
                    let (function_builder, addr) = expr func function_builder (SId("#d")) in
                    let (function_builder, k')   = expr func function_builder (SId("#k")) in

                    let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" function_builder in
                    let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
                    let ltyp1_ptr = L.build_load addr_t1 "t1*" function_builder in
                    let ltyp2_ptr = L.build_load addr_t2 "t2*" function_builder in
                    let ltyp1 = L.type_of (L.build_load ltyp1_ptr "t1" function_builder) in
                    let ltyp2 = L.type_of (L.build_load ltyp2_ptr "t2" function_builder) in
                    let (function_builder, k_data) = make_addr k' ltyp1 false function_builder func in
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

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let v_data_load = L.build_call func [|addr;k'|] "dget" builder in
            (builder, v_data_load)

        | SFunCall((t,SId("lremove")), [(t_l, l)]) ->
            expr parent_func builder (SFunCall((t,SId("lremove")), [(t_l, l); ([A.Int],SIntLit(0))]))

        | SFunCall((_,SId("lremove")), [(_, l); (_,n)]) ->
            let (builder, addr) = expr parent_func builder l in
            let (builder, n') = expr parent_func builder n in
            let addr_typ = L.type_of addr in
            let f_name = "lremove" ^ (str_of_ltyp addr_typ) in

            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let (func, function_builder) = make_func f_name [(addr_typ,"#l");(i32_t,"#n")] addr_typ in

                    init_params func f_name function_builder;

                    let (function_builder, addr) = expr func function_builder (SId("#l")) in
                    let (function_builder, n') = expr func function_builder (SId("#n")) in

                    let addr_t = L.build_in_bounds_gep addr [|zero;zero|] "listt" function_builder in
                    let addr_head = L.build_in_bounds_gep addr [|zero;one|] "listhead" function_builder in
                    let head_node = L.build_load addr_head "headnode" function_builder in
                    let head_node' = L.build_call ll_remove_func [|head_node;n'|] "headnode_" function_builder in
                    ignore(L.build_store head_node' addr_head function_builder);
                    ignore(L.build_ret addr function_builder);

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let addr' = L.build_call func [|addr;n'|] "lremove" builder in
            (builder, addr')

        | SFunCall((_,SId("dremove")), [(_, dict); (_,k)]) ->
            let (builder, addr) = expr parent_func builder dict in
            let (builder, k') = expr parent_func builder k in
            let addr_typ = L.type_of addr in
            let f_name = "dremove" ^ (str_of_ltyp addr_typ) in
            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let k_typ = L.type_of k' in
                    let (func, function_builder) = make_func f_name [(addr_typ,"#d");(k_typ,"#k")] addr_typ in

                    init_params func f_name function_builder;

                    (* building dremove function *)
                    let (function_builder, addr) = expr func function_builder (SId("#d")) in
                    let (function_builder, k')   = expr func function_builder (SId("#k")) in

                    let addr_t1 = L.build_in_bounds_gep addr [|zero;zero|] "dictt1" function_builder in
                    let addr_t2 = L.build_in_bounds_gep addr [|zero;one|] "dictt2" function_builder in
                    let addr_ht = L.build_in_bounds_gep addr [|zero;two|] "dictht" function_builder in
                    let ltyp1_ptr = L.build_load addr_t1 "t1*" function_builder in
                    let ltyp2_ptr = L.build_load addr_t2 "t2*" function_builder in
                    let ltyp1 = L.type_of (L.build_load ltyp1_ptr "t1" function_builder) in
                    let ltyp2 = L.type_of (L.build_load ltyp2_ptr "t2" function_builder) in
                    let (function_builder, k_data) = make_addr k' ltyp1 false function_builder func in
                    let c_k_data = L.build_bitcast k_data string_t "ckdata" function_builder in
                    let ht = L.build_load addr_ht "ht" function_builder in
                    let ht' = L.build_call ht_remove_func [|ht;c_k_data|] "ht_" function_builder in
                    (if L.is_null ht' then raise (Failure "malloc failed") else ());
                    ignore(L.build_store ht' addr_ht function_builder);
                    ignore(L.build_ret addr function_builder);

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let addr = L.build_call func [|addr;k'|] "dremove" builder in
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
            let arg_func_typ = L.type_of arg_func in
            let f_name = "sfold" ^ (str_of_ltyp (L.element_type arg_func_typ)) in

            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (

                    let accum_typ = L.type_of a' in
                    let formals = [(arg_func_typ,"#f");(accum_typ,"#a");(string_t,"#s")] in
                    let (func, function_builder) = make_func f_name (add_parent_vars_to_formals formals) accum_typ in

                    init_params func f_name function_builder;

                    let arg_func_params = L.params arg_func in

                    let (function_builder, arg_func) = expr func function_builder (SId("#f")) in
                    let (function_builder, a') = expr func function_builder (SId("#a")) in
                    let (function_builder, addr) = expr func function_builder (SId("#s")) in

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

                    let actuals = add_parent_vars_to_actuals arg_func_params [a;c_str] in
                    let actuals_arr = Array.of_list actuals in
                    let a = L.build_call arg_func actuals_arr "accumresult" body_builder in

                    ignore(L.build_store a accum_addr body_builder);
                    let i = L.build_add i one "i" body_builder in
                    ignore(L.build_store i i_addr body_builder);

                    let merge_bb = L.append_block context "merge" func in

                    ignore(L.build_br cond_bb function_builder);
                    ignore(L.build_br cond_bb body_builder);
                    ignore(L.build_cond_br cond body_bb merge_bb cond_builder);

                    let function_builder = L.builder_at_end context merge_bb in
                    let accum_final = L.build_load accum_addr "sfoldaccum" function_builder in

                    ignore(L.build_ret accum_final function_builder);

                    cleanup_func_vars func;
                    Hashtbl.add func_tbl f_name func;

                    func
                )
            ) in

            let actuals = add_parent_vars_to_actuals (L.params func) [arg_func;a';addr] in
            let actuals_arr = Array.of_list actuals in
            let accum_final = L.build_call func actuals_arr "sfoldaccumfinal" builder in
            (builder, accum_final)

        | SFunCall((_,SId("lfold")), [(_,f);(_,a);(_,l)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, a') = expr parent_func builder a in
            let (builder, addr) = expr parent_func builder l in
            let f_name_suf = (str_of_ltyp (L.type_of a')) ^ (str_of_ltyp (L.type_of addr)) in
            let f_name = "lfold" ^ f_name_suf ^ (string_of_int (Array.length (L.params arg_func))) in

            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let func = make_lfold_func f_name arg_func a' addr in
                    Hashtbl.add func_tbl f_name func;
                    func
                )
            ) in

            let actuals = add_parent_vars_to_actuals (L.params func) [arg_func;a';addr] in
            let actuals_arr = Array.of_list actuals in
            let accum_final = L.build_call func actuals_arr "lfoldaccumfinal" builder in
            (builder, accum_final)

        | SFunCall((_,SId("dfold")), [(_,f);(_,a);(_,d)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, a') = expr parent_func builder a in
            let (builder, addr) = expr parent_func builder d in
            let f_name_suf = (str_of_ltyp (L.type_of a')) ^ (str_of_ltyp (L.type_of addr)) in
            let f_name = "dfold" ^ f_name_suf ^ (string_of_int (Array.length (L.params arg_func))) in

            let func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let func = make_dfold_func f_name arg_func a' addr in
                    Hashtbl.add func_tbl f_name func;
                    func
                )
            ) in


            let actuals = add_parent_vars_to_actuals (L.params func) [arg_func;a';addr] in
            let actuals_arr = Array.of_list actuals in

            let accum_final = L.build_call func actuals_arr "dfoldaccumfinal" builder in
            (builder, accum_final)

        | SFunCall((t,SId("lmap")), [(typlist_f,f);(typlist_l,l)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, addr) = expr parent_func builder l in

            (* initiate empty list *)
            let typ = match (assc_typ_of_typlist typlist_l) with A.List(t) -> t | _ -> raise (Failure internal_err) in
            let (builder, new_list_addr) = expr parent_func builder (SListLit(typ,[])) in

            let addr_typ = L.type_of addr in
            let arg_func_name = L.value_name arg_func in
            let ltyp = ltyp_of_typ typ in

            let wrapper_f_name = "foldwrapper" ^ arg_func_name in

            let wrapper_func = (try Hashtbl.find func_tbl wrapper_f_name
                with Not_found -> (
                    let formals = [(addr_typ,"#a");(ltyp,"#e")] in
                    let (wrapper_func, function_builder) = make_func wrapper_f_name (add_parent_vars_to_formals formals) addr_typ in

                    init_params wrapper_func wrapper_f_name function_builder;

                    let arg_func_params = L.params arg_func in

                    let (function_builder, a') = expr wrapper_func function_builder (SId("#a")) in
                    let (function_builder, e') = expr wrapper_func function_builder (SId("#e")) in

                    let ladd_f_name = "ladd" ^ str_of_ltyp addr_typ in
                    let ladd_func = (try Hashtbl.find func_tbl ladd_f_name
                        with Not_found -> (
                            let ladd_func = make_ladd_func ladd_f_name addr e' in
                            Hashtbl.add func_tbl ladd_f_name ladd_func;
                            ladd_func
                        )
                    ) in

                    let actuals = add_parent_vars_to_actuals arg_func_params [e'] in
                    let actuals_arr = Array.of_list actuals in
                    let e' = L.build_call arg_func actuals_arr "e" function_builder in
                    let addr' = L.build_call ladd_func [|a';e';max_int|] "ladd" function_builder in
                    ignore(L.build_ret addr' function_builder);

                    cleanup_func_vars wrapper_func;
                    Hashtbl.add func_tbl wrapper_f_name wrapper_func;

                    wrapper_func
                )
            ) in

            let f_name_suf = (str_of_ltyp addr_typ) ^ (str_of_ltyp addr_typ) in
            let f_name = "lfold" ^ f_name_suf ^ (string_of_int (Array.length (L.params wrapper_func))) in

            let lfold_func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let lfold_func = make_lfold_func f_name wrapper_func new_list_addr addr in
                    Hashtbl.add func_tbl f_name lfold_func;
                    lfold_func
                )
            ) in

            let actuals = add_parent_vars_to_actuals (L.params lfold_func) [wrapper_func;new_list_addr;addr] in
            let actuals_arr = Array.of_list actuals in
            let accum_final = L.build_call lfold_func actuals_arr "lmapaccumfinal" builder in
            (builder, accum_final)

        | SFunCall((t,SId("dmap")), [(typlist_f,f);(typlist_d,d)]) ->
            let (builder, arg_func) = expr parent_func builder f in
            let (builder, addr) = expr parent_func builder d in

            (* initiate empty list *)
            let (t1, t2) = match (assc_typ_of_typlist typlist_d) with A.Dict(t1,t2) -> (t1,t2) | _ -> raise (Failure internal_err) in
            let (builder, new_dict_addr) = expr parent_func builder (SDictLit(t1,t2,[])) in

            let addr_typ = L.type_of addr in
            let arg_func_name = L.value_name arg_func in
            let ltyp1 = ltyp_of_typ t1 in
            let ltyp2 = ltyp_of_typ t2 in

            let wrapper_f_name = "foldwrapper" ^ arg_func_name in
            let wrapper_func = (try Hashtbl.find func_tbl wrapper_f_name
                with Not_found -> (
                    let formals = [(addr_typ,"#a");(ltyp1,"#k");(ltyp2,"#v")] in
                    let (wrapper_func, function_builder) = make_func wrapper_f_name (add_parent_vars_to_formals formals) addr_typ in

                    init_params wrapper_func wrapper_f_name function_builder;

                    let arg_func_params = L.params arg_func in

                    let (function_builder, a') = expr wrapper_func function_builder (SId("#a")) in
                    let (function_builder, k') = expr wrapper_func function_builder (SId("#k")) in
                    let (function_builder, v') = expr wrapper_func function_builder (SId("#v")) in

                    let dadd_f_name = "dadd" ^ str_of_ltyp addr_typ in
                    let dadd_func = (try Hashtbl.find func_tbl dadd_f_name
                        with Not_found -> (
                            let dadd_func = make_dadd_func dadd_f_name addr k' v' in
                            Hashtbl.add func_tbl dadd_f_name dadd_func;
                            dadd_func
                        )
                    ) in

                    let actuals = add_parent_vars_to_actuals arg_func_params [k';v'] in
                    let actuals_arr = Array.of_list actuals in
                    let v' = L.build_call arg_func actuals_arr "v" function_builder in
                    let addr' = L.build_call dadd_func [|a';k';v'|] "dadd" function_builder in
                    ignore(L.build_ret addr' function_builder);

                    cleanup_func_vars wrapper_func;
                    Hashtbl.add func_tbl wrapper_f_name wrapper_func;

                    wrapper_func
                )
            ) in


            let f_name_suf = (str_of_ltyp addr_typ) ^ (str_of_ltyp addr_typ) in
            let f_name = "dfold" ^ f_name_suf ^ (string_of_int (Array.length (L.params wrapper_func))) in

            let dfold_func = (try Hashtbl.find func_tbl f_name
                with Not_found -> (
                    let dfold_func = make_dfold_func f_name wrapper_func new_dict_addr addr in
                    Hashtbl.add func_tbl f_name dfold_func;
                    dfold_func
                )
            ) in

            let actuals = add_parent_vars_to_actuals (L.params dfold_func) [wrapper_func;new_dict_addr;addr] in
            let actuals_arr = Array.of_list actuals in
            let accum_final = L.build_call dfold_func actuals_arr "dmapaccumfinal" builder in
            (builder, accum_final)

        | SFunCall((_,SId("dkeys")), [(typlist,d)]) ->
            let (builder, d_addr) = expr parent_func builder d in
            let t1 = match (assc_typ_of_typlist typlist) with A.Dict(t1,_) -> t1 | _ -> raise (Failure internal_err) in
            let addr_ht = L.build_in_bounds_gep d_addr [|zero;two|] "dictht" builder in
            let ht = L.build_load addr_ht "ht" builder in

            (* making empty list for keys *)
            let (builder, l_addr) = expr parent_func builder (SListLit(t1,[])) in
            let l_addr_head = L.build_in_bounds_gep l_addr [|zero;one|] "listhead" builder in

            let keys_list_head = L.build_call ht_keys_list_func [|ht|] "keyslisthead" builder in
            ignore(L.build_store keys_list_head l_addr_head builder);

            (builder, l_addr)

        | SFunCall((_,SId("rematch")), [(_,r);(_,s)]) ->
            let (builder, regex) = expr parent_func builder r in
            let (builder, addr) = expr parent_func builder s in
            let out = L.build_call re_match_func [|regex;addr|] "rematch" builder in
            (builder, out)

        | SFunCall((_,SId("resub")), [(_,r);(_,s);(_,t);(_,n)]) ->
            let (builder, regex) = expr parent_func builder r in
            let (builder, s_addr) = expr parent_func builder s in
            let (builder, t_addr) = expr parent_func builder t in
            let (builder, n') = expr parent_func builder n in
            let new_s_addr = L.build_call re_sub_func [|regex;s_addr;t_addr;n'|] "resub" builder in
            (builder, new_s_addr)

        | SAssign(v, (_,e)) ->
            let (builder, e') = expr parent_func builder e in
            L.set_value_name v e';
            Hashtbl.add var_tbl v (e', parent_func);
            (builder, e')

        | SId(id) ->
            let f_name = L.value_name parent_func in
            let e' = try
                    let (e', _) = Hashtbl.find var_tbl id in
                    e'
                with Not_found -> (
                    match L.lookup_function id the_module with
                        Some f -> f
                        | None -> raise (Failure ("ID not found: " ^ id))
                )
            in
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

            let make_expr_cond_block (blocks, i) (sexpr_or_def, _) then_block =
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

            let make_typ_cond_block (blocks, i) (typ_or_def, _) then_block =
                let cond_bb = L.append_block context ("cond" ^ string_of_int i) parent_func in
                let cond_builder = L.builder_at_end context cond_bb in
                ignore(match typ_or_def with
                    A.TypMatch t ->
                        let (cond_builder, cond) = expr parent_func cond_builder (STypComp(m.sminput, t)) in
                        ignore(L.build_cond_br cond then_block (List.hd blocks) cond_builder);

                    | A.DefaultTyp ->
                        ignore(L.build_br then_block cond_builder);
                );
                (cond_bb::blocks, i - 1)
            in

            let cond_blocks = (match m.smatchlist with
                SValMatchList(l)->
                    let (blocks, _) = List.fold_left make_block ([], 0) l in
                    (* blocks is in reverse order *)
                    let (cond_blocks, _) = List.fold_left2 make_expr_cond_block ([], (List.length blocks - 1)) (List.rev l) blocks
                    in
                    cond_blocks

                | STypMatchList(l) ->
                    let (blocks, _) = List.fold_left make_block ([], 0) l in
                    let (cond_blocks, _) = List.fold_left2 make_typ_cond_block ([], (List.length blocks - 1)) (List.rev l) blocks
                    in
                    cond_blocks
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
            let formals = List.map (fun (t,n) -> (ltyp_of_typ t, n)) f.sformals in
            let ret_typ = ltyp_of_typ f.sftyp in

            let (func, function_builder) = make_func f_name (add_parent_vars_to_formals formals) ret_typ in

            init_params func f_name function_builder;

            let (_, function_builder, function_out) = List.fold_left stmt (func, function_builder, zero) f.sfblock in
            ignore(L.build_ret function_out function_builder);

            cleanup_func_vars func;

            (builder, func)

        | SFunCall((typlist,e), l) ->
            let make_actuals (builder, actuals) (_, e) =
                let (builder, e') = expr parent_func builder e in
                (builder, e'::actuals)
            in
            let (builder, actuals) = List.fold_left make_actuals (builder, []) l in
            let (builder, func) = expr parent_func builder e in

            let params = L.params func in
            let actuals = add_parent_vars_to_actuals (L.params func) (List.rev actuals) in
            let actuals_arr = Array.of_list (actuals) in
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
            let t' = assc_typ_of_typlist [t] in
            let ut = match t' with A.UserTypDef(ut) -> ut | _ -> raise (Failure (internal_err)) in
            let (utd_typ, name_pos) = Hashtbl.find utd_typs ut in
            let name = match L.struct_name utd_typ with Some n -> n | None -> raise (Failure (internal_err)) in
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
            Hashtbl.add var_tbl v (addr, parent_func);
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

    and stmt (func, builder, out) = function
        SExprStmt(e) ->
            let (builder', out') = expr func builder (snd e) in
            (func, builder', out')

        | STypDecl(v, l) ->
            let make_typ (n, t) =
                Hashtbl.add ut_typs n (ltyp_of_typ t);
                ignore(L.define_global n (L.const_int i32_t !ut_i) the_module);
                ut_i := !ut_i + 1;
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
    (*
    let print_block b =
        print_endline(L.string_of_llvalue (L.value_of_block b));
    in
    L.iter_blocks print_block main;
    *)
    ignore(L.build_call free_malloc_addrs_func [||] "" builder);
    ignore(L.build_ret (L.const_int i32_t 0) builder);
    the_module
