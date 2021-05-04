(* Top level of the CLL compiler *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
        let action = ref Compile in
        let set_action a () = action := a in
        let speclist = [
            ("-a", Arg.Unit (set_action Ast), "Print the AST");
            ("-s", Arg.Unit (set_action Sast), "Print the SAST");
            ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
            ("-c", Arg.Unit (set_action Compile), "Compile the program");
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
                raise (Failure (s ^ "\n"))
            | Failure(msg) ->
                let s = ("!!!ERROR!!! line " ^ string_of_int !Scanner.line_num ^ ": " ^ msg ^ "\n")
                in
                raise (Failure (s ^ "\n"))
        in
        match !action with
            Ast -> print_string (Ast.string_of_program ast)
            |_ ->
                let (env, sast) = try Semant.check_ast ast
                    with Failure(msg) ->
                        let file_out = !filename ^ ".ast" in
                        let log = open_out file_out in
                        Printf.fprintf log "%s" (Ast.string_of_program ast);
                        close_out log;
                        let s = (msg ^ "\n(Check " ^ file_out ^ " for line numbers)\n")
                        in
                        raise (Failure (s ^ "\n"))
                in
                match !action with
                Ast -> ()
                | Sast -> print_string (Sast.string_of_sprogram sast)
                | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate env sast))
                | Compile -> let m = Codegen.translate env sast in
	                Llvm_analysis.assert_valid_module m;
	                print_string (Llvm.string_of_llmodule m)
