open Ast
open Eio

exception ToManyArguments of int

let rec run : debug:int -> rco_expr -> ctail =
 fun ~debug e ->
  let r = explicate_tail e in
  if debug >= 2 then traceln "[pass] explicate control\n%s" (show_ctail r);
  r

and explicate_tail : rco_expr -> ctail =
 fun e ->
  match e with
  | `Bool true -> Return (`CInt 1)
  | `Bool false -> Return (`CInt 0)
  | `Int i -> Return (`CInt i)
  | `Var x -> Return (`CVar x)
  | `Let (x, t, body) -> explicate_assign t x (explicate_tail body)
  | `If (c, t, f) -> explicate_pred c (explicate_tail t) (explicate_tail f)
  | `Prim (Not, [ a ]) -> Return (`Not (explicate_atom a))
  | `Prim (Add, [ a; b ]) -> Return (`Add (explicate_atom a, explicate_atom b))
  | `Prim (Sub, [ a; b ]) -> Return (`Sub (explicate_atom a, explicate_atom b))
  | `Prim (And, [ a; b ]) -> Return (`And (explicate_atom a, explicate_atom b))
  | `Prim (Or, [ a; b ]) -> Return (`Or (explicate_atom a, explicate_atom b))
  | `Prim (_, es) -> raise (ToManyArguments (List.length es))

and explicate_assign : rco_expr -> string -> ctail -> ctail =
 fun e x cont ->
  match e with
  | `Bool true -> Seq (Assign (x, `CInt 1), cont)
  | `Bool false -> Seq (Assign (x, `CInt 0), cont)
  | `Int i -> Seq (Assign (x, `CInt i), cont)
  | `Var x -> Seq (Assign (x, `CVar x), cont)
  | `Let (x2, t, body) ->
      let body' = explicate_assign body x cont in
      explicate_assign t x2 body'
  | `If (c, t, f) -> explicate_pred c (explicate_tail t) (explicate_tail f)
  | `Prim (Not, [ a ]) -> Seq (Assign (x, `Not (explicate_atom a)), cont)
  | `Prim (Add, [ a; b ]) ->
      Seq (Assign (x, `Add (explicate_atom a, explicate_atom b)), cont)
  | `Prim (Sub, [ a; b ]) ->
      Seq (Assign (x, `Sub (explicate_atom a, explicate_atom b)), cont)
  | `Prim (And, [ a; b ]) ->
      Seq (Assign (x, `And (explicate_atom a, explicate_atom b)), cont)
  | `Prim (Or, [ a; b ]) ->
      Seq (Assign (x, `Or (explicate_atom a, explicate_atom b)), cont)
  | `Prim (_, es) -> raise (ToManyArguments (List.length es))

and explicate_pred : rco_expr -> ctail -> ctail -> ctail =
 fun e if_true if_false -> failwith "TODO"

and explicate_atom : atom -> catom =
 fun e ->
  match e with
  | `Bool true -> `CInt 1
  | `Bool false -> `CInt 0
  | `Int i -> `CInt i
  | `Var x -> `CVar x
