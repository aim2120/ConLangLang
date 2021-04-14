(* Top level of the CLL compiler *)

let () =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    (*
    print_string (Ast.string_of_program ast)
    *)
    let sast = Semant.check_ast ast in
    print_string (Sast.string_of_sprogram sast)
    (*
    *)
