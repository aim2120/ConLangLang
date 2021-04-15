(* Top level of the CLL compiler *)

type action = Ast | Sast | LLVM_IR

let () =
    let action = ref LLVM_IR in
    let set_action a () = action := a in
    let speclist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Print the SAST");
        ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ] in
    let usage_msg = "usage: ./cll.native [-a|-s|-l] [file.cll]" in
    let channel = ref stdin in
    let filename = ref "" in
    Arg.parse speclist (fun file -> filename := file; channel := open_in file;) usage_msg;
    try 
        let lexbuf = Lexing.from_channel !channel in
        let ast = Parser.program Scanner.token lexbuf
        in
        match !action with
            Ast -> 
                print_string (Ast.string_of_program ast)
            | Sast ->
                (try
                    let sast = Semant.check_ast ast in
                    print_string (Sast.string_of_sprogram sast)
                with Failure(msg) ->
                    let file_out = !filename ^ ".ast" in
                    let log = open_out file_out in
                    Printf.fprintf log "%s" (Ast.string_of_program ast);
                    close_out log;
                    print_string (msg ^ "\n(Check " ^ file_out ^ " for line numbers)\n");
                )
            | LLVM_IR ->
                ()
    with
    Parsing.Parse_error ->
        print_string ("!!!ERROR!!! line " ^ string_of_int !Scanner.line_num ^ ": parsing error\n")
    | Failure(msg) ->
        print_string ("!!!ERROR!!! line " ^ string_of_int !Scanner.line_num ^ ": " ^ msg ^ "\n")
