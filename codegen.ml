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
    let list_types = Hashtbl.create 10 in
    let dict_types = Hashtbl.create 10 in
    let not_impl = " not implemented" in
    let context = L.global_context () in

    (* Create the LLVM compilation module into which
       we will generate code *)
    let the_module = L.create_module context "ConLangLang" in

    (* Get types from the context *)
    let i32_t         = L.i32_type    context
    in let i1_t       = L.i1_type     context
    in let i8_t       = L.i8_type     context
    in let float_t    = L.double_type context
    in let string_t   = L.pointer_type i8_t
    in let void_t     = L.void_type   context
    in

    (* Return the LLVM type for a MicroC type *)
    let rec typ_to_ltyp = function
          A.Int    -> i32_t
        | A.Bool   -> i1_t
        | A.Float  -> float_t
        | A.String -> string_t
        | A.Null   -> void_t
        | A.List(t)->
            let ltyp = typ_to_ltyp t in
            let ltyp_s = List.hd (String.split_on_char ' ' (L.string_of_lltype ltyp)) in
            let list_name = "list" ^ ltyp_s in
            if Hashtbl.mem list_types list_name then (
                let list_t = Hashtbl.find list_types list_name in
                list_t
            ) else (
                let list_t = L.named_struct_type context list_name in
                L.struct_set_body list_t [| L.pointer_type ltyp; L.pointer_type list_t |] false;
                Hashtbl.add list_types list_name list_t;
                list_t
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

    (* printf *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in
    let str_format = L.build_global_stringptr "%s\n" "fmt" builder in

    let rec expr builder = function
        SIntLit(i) -> L.const_int i32_t i
        | SFloatLit(f) -> L.const_float_of_string float_t f
        | SBoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
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
            String.iteri store_char s;
            let addr_end = L.build_in_bounds_gep addr [|L.const_int i8_t (len - 1)|] "end" builder in
            ignore(L.build_store (L.const_int i8_t 0) addr_end builder);
            addr
        | SListLit(t, l) ->
            let list_t = typ_to_ltyp (A.List(t)) in
            let zero = L.const_int i32_t 0 in
            let one = L.const_int i32_t 1 in
            let l' = List.map (fun e -> snd e) l in
            let prefix = "l" in
            let data = "d" in
            let add_node i e =
                let addr = L.build_alloca list_t (prefix ^ string_of_int i) builder in
                (addr, e)
            in
            let node_addrs = List.mapi add_node l' in
            let node_addrs_rev = List.rev node_addrs in
            let null_addr = L.build_alloca list_t (prefix ^ string_of_int (List.length node_addrs)) builder in
            ignore(L.build_store (L.const_null (list_t)) null_addr builder);
            let make_node next_node_addr (node_addr, e) =
                (* list_t = { void *data; list_t *next; } *)
                let node_data = expr builder e in
                let node_data_addr = match t with
                    A.Int | A.Float | A.Bool | A.String  ->
                        let addr = L.build_alloca (typ_to_ltyp t) "" builder in
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
                node_addr
            in
            ignore(List.fold_left make_node null_addr node_addrs_rev);
            fst (List.hd node_addrs)
        | SFunCall("sprint", [sexpr]) ->
            let e = snd sexpr in
            L.build_call printf_func [| str_format; (expr builder e) |] "printf" builder
        | _ -> raise (Failure ("expr" ^ not_impl))
        (*
        *)
        (*
        | SReLit(r) -> L.const_string context r
        | SDictLit(t1, t2, d) -> ()
        | SFunLit(f) -> ()
        | SNullExpr -> ()
        | SBinop(e1, o, e2) -> ()
        | SUnop(o, e) -> ()
        | SChildAcc(e, s) -> ()
        | SCast(t, e) -> ()
        | SAssign(v, e) -> ()
        | STypDefAssign(t, v, l) -> ()
        | SMatch(m) -> ()
        | SIfElse(i) -> ()
        | SWhile(w) -> ()
        | SId(v) -> ()
        | SUTDId(v) -> ()
        | SExpr(e) -> ()
                    *)
    in
    let rec stmt (builder, v) = function
        SExprStmt(e) ->
            let v = expr builder (snd e) in
            (builder, v)
        | _      -> raise (Failure ("stmt" ^ not_impl))
    in
    let _ = List.fold_left stmt (builder, L.const_int i32_t 0) sast (* dummy llval *)
    in
    ignore(L.build_ret (L.const_int i32_t 0) builder);
    the_module
