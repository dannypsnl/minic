open Lexing

let catcher f lexbuf =
  try f Lexer.token lexbuf with
  | Grammar.Error ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "failed to parse `%s`"
        (Lexing.lexeme lexbuf)
  | Lexer.SyntaxError token ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.fatalf ~loc Parse_error "unrecognized token `%s`"
      @@ String.escaped token

let parse_channel filename ch =
  Reporter.tracef "when parsing file `%s`" filename @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  catcher Grammar.entry lexbuf

(* from a file, build out a module *)
let parse_file filename =
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch
