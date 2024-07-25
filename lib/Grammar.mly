%{
  [@@@coverage exclude_file]
  open Ast
%}

%token <string> IDENT
%token <int> INT
(* keywords *)
%token LET
       IF
       THEN
       ELSE
       TRUE
       FALSE
       IN
       WHILE
       END
%token LPAREN RPAREN
       ASSIGN
       SEMICOLON
       EQ
       LT
       LE
       ADD
       SUB
       NOT
       AND
       OR
%token EOF

%start <surface_expr> entry
%%

let parens(p) == delimited(LPAREN, p, RPAREN)

let op :=
  | EQ; { EQ }
  | LT; { LT }
  | LE; { LE }
  | ADD; { Add }
  | SUB; { Sub }
  | AND; { And }
  | OR; { Or }

let unary_op :=
  | NOT; { Not }

let factor :=
  | TRUE; { `Bool true }
  | FALSE; { `Bool false }
  | n=INT; { `Int n }
  | n=IDENT; { `Var n }
  | a=tm; SEMICOLON; b=tm; { `Seq (a, b) }
  | parens(tm)
let tm :=
  | op=unary_op; e=tm; { `UPrim (op, e) }
  | l=factor; op=op; r=tm; { `Prim (op, l, r) }
  | LET; n=IDENT; ASSIGN; e=tm; IN; b=tm; { `Let (n, e, b) }
  | IF; p=tm; THEN; t=tm; ELSE; e=tm; { `If (p, t, e) }
  | WHILE; p=tm; b=tm; END; { `While (p, b) }
  | n=IDENT; ASSIGN; e=tm; { `Set (n, e) }
  | factor

let entry :=
  | tm=tm; EOF; { tm }
