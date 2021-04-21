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
    let not_impl = "not implemented" in
    let context    = L.global_context () in
    
    (* Create the LLVM compilation module into which
       we will generate code *)
    let the_module = L.create_module context "ConLangLang" in
    
    (* Get types from the context *)
    let i32_t      = L.i32_type    context
    and i8_t       = L.i8_type     context
    and i1_t       = L.i1_type     context
    and float_t    = L.double_type context
    and void_t     = L.void_type   context
    and list_t = L.named_struct_type context "list"
    in
    (* Return the LLVM type for a MicroC type *)
    let ltyp_of_typ = function
          A.Int    -> i32_t
        | A.Bool   -> i1_t
        | A.Float  -> float_t
        | A.Null   -> void_t
        | A.List(_)-> list_t
        (*
        | A.UserTyp(ut)  -> let (_, at) = StringMap.find ut env.tsym in ltype_of_typ at
        *)
        | _        -> raise (Failure not_impl)
(*
        | A.String ->
        | A.Regex  ->
        | A.List(t) ->
        | A.Dict(t1,t2)  ->
        | A.Fun(f,t)  ->
        | A.UserTypDef(u) ->
*)
    in
    let rec expr builder = function
          SIntLit(i) -> L.const_int i32_t i
        | SFloatLit(f) -> L.const_float_of_string float_t f
        | SBoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
        | SStrLit(s) -> L.const_string context s
        | SReLit(r) -> L.const_string context r
        | _ -> raise (Failure not_impl)
        (*
        | SListLit(t, l) ->
                let t' = ltyp_of_typ t in
                let prefix = "l" in
                let make_node e i =
                    let node_addr = L.build_alloca list_t (prefix ^ string_of_int i) builder in 
                    ignore(L.build_store (expr builder e) node_addr builder);
                    let node_addr_ = L.build_in_bounds_gep node_addr [|L.const_int i32_t 0; L.const_int i32_t 1|] "next" builder in
                    let next_node_addr = L.build_alloca list_t (prefix ^ string_of_int i) builder in 
                    ignore(L.build_store (next_node_addr) node_addr_ builder);
                    node_addr
                in make_node (snd (List.hd l)) 0
        | _ -> raise (Failure not_impl)
        *)
        (*
        | SDictLit(t1, t2, d) -> ()
        | SFunLit(f) -> ()
        | SNullExpr -> ()
        | SBinop(e1, o, e2) -> ()
        | SUnop(o, e) -> ()
        | SChildAcc(e, s) -> ()
        | SCast(t, e) -> ()
        | SAssign(v, e) -> ()
        | STypDefAssign(t, v, l) -> ()
        | SFunCall(v, l) -> ()
        | SMatch(m) -> ()
        | SIfElse(i) -> ()
        | SWhile(w) -> ()
        | SId(v) -> ()
        | SUTDId(v) -> ()
        | SExpr(e) -> ()
                    *)
    in
    let rec stmt builder = function
        SExprStmt(e) ->
            let v = expr builder (snd e) in
            let _ = L.build_ret v builder in
            builder
        | _      -> raise (Failure not_impl)
    in
    let last_stmt = List.hd (List.rev sast) in
    let return_t = ltyp_of_typ (match last_stmt with SExprStmt(e) -> List.hd (fst e) | _ -> raise (Failure "weird")) in
    let func_t = L.function_type return_t [||] in
    let main = L.define_function "main" func_t the_module in
    let builder = L.builder_at_end context (L.entry_block main) in
    let builder = List.fold_left stmt builder sast in
    the_module
