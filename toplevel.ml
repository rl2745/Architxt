open Ast

let () =
  let lex_buf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in 
  (*print_string (Ast.string_of_program ast)*) 
  (*for testing, just have it say okay to make it easier for the script to process*)
  print_string ("OKAY")
 
