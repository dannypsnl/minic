{
  exception SyntaxError of string
  let ident str = Grammar.IDENT str
  let int s = Grammar.INT (int_of_string s)
  let illegal str = raise @@ SyntaxError str

  let dbg str = Format.printf "%s\n" str; flush stdout

  let return _lexbuf tok = tok
}

let digit = ['0'-'9']
let int = '-'? digit+

let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha|digit|'_'|'-'|'?')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token =
  parse
  | "#" { comment lexbuf }
  | "let" { return lexbuf @@ Grammar.LET }
  | "in" { return lexbuf @@ Grammar.IN }
  | "true" { return lexbuf @@ Grammar.TRUE }
  | "false" { return lexbuf @@ Grammar.FALSE }
  | "if" { return lexbuf @@ Grammar.IF }
  | "then" { return lexbuf @@ Grammar.THEN }
  | "else" { return lexbuf @@ Grammar.ELSE }
  | "while" { return lexbuf @@ Grammar.WHILE }
  | "end" { return lexbuf @@ Grammar.END }
  | ident { return lexbuf @@ ident (Lexing.lexeme lexbuf) }
  | int { return lexbuf @@ int (Lexing.lexeme lexbuf) }
  | ":=" { return lexbuf @@ Grammar.ASSIGN }
  | ";" { return lexbuf @@ Grammar.SEMICOLON }
  | "==" { return lexbuf @@ Grammar.EQ }
  | "<" { return lexbuf @@ Grammar.LT }
  | "<=" { return lexbuf @@ Grammar.LE }
  | "+" { return lexbuf @@ Grammar.ADD }
  | "-" { return lexbuf @@ Grammar.SUB }
  | "!" { return lexbuf @@ Grammar.NOT }
  | "&&" { return lexbuf @@ Grammar.AND }
  | "||" { return lexbuf @@ Grammar.OR }
  | "(" { return lexbuf @@ Grammar.LPAREN }
  | ")" { return lexbuf @@ Grammar.RPAREN }
  | whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { comment lexbuf }
