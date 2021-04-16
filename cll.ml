(* Top level of the CLL compiler *)

type action = Ast | Sast | LLVM_IR

let () =
    try
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
        let ast = try
                let lexbuf = Lexing.from_channel !channel in
                Parser.program Scanner.token lexbuf
            with Parsing.Parse_error ->
                let s = ("!!!ERROR!!! line " ^ string_of_int !Scanner.line_num ^ ": parsing error\n")
                in
                raise (Failure s)
            | Failure(msg) ->
                let s = ("!!!ERROR!!! line " ^ string_of_int !Scanner.line_num ^ ": " ^ msg ^ "\n")
                in
                raise (Failure s)
        in
        match !action with
            Ast -> print_string (Ast.string_of_program ast)
            |_ ->
                let sast = try Semant.check_ast ast
                    with Failure(msg) ->
                        let file_out = !filename ^ ".ast" in
                        let log = open_out file_out in
                        Printf.fprintf log "%s" (Ast.string_of_program ast);
                        close_out log;
                        let s = (msg ^ "\n(Check " ^ file_out ^ " for line numbers)\n")
                        in
                        raise (Failure s)
                in
                match !action with
                Ast -> ()
                | Sast -> print_string (Sast.string_of_sprogram sast)
                | LLVM_IR -> Codegen.translate sast
    with Failure(msg) -> print_string msg
