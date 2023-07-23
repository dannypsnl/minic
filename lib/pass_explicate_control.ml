open Ast

exception InvalidTail of expr
exception InvalidAtom of expr
exception ToManyArguments of int

let rec run : expr -> ctail = fun e -> explicate_tail e

and explicate_tail : expr -> ctail =
 fun e ->
  match e with
  | Int i -> Return (`CInt i)
  | Var x -> Return (`CVar x)
  | Let (x, t, body) -> explicate_assign t x (explicate_tail body)
  | Prim (op, [ a; b ]) ->
      Return (`CPrim (op, explicate_atom a, explicate_atom b))
  | Prim (_, es) -> raise (ToManyArguments (List.length es))

and explicate_assign : expr -> string -> ctail -> ctail =
 fun e x cont ->
  match e with
  | Int i -> Seq (Assign (x, `CInt i), cont)
  | Var x -> Seq (Assign (x, `CVar x), cont)
  | Let (x, t, body) ->
      let body' = explicate_assign body x cont in
      explicate_assign t x body'
  | Prim (op, [ a; b ]) ->
      Seq (Assign (x, `CPrim (op, explicate_atom a, explicate_atom b)), cont)
  | Prim (_, es) -> raise (ToManyArguments (List.length es))

and explicate_atom : expr -> catom =
 fun e ->
  match e with
  | Int i -> `CInt i
  | Var x -> `CVar x
  | _ -> raise (InvalidAtom e)
