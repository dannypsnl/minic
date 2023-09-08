open Ast
open Eio

exception BadExpr of surface_expr

let rec split : 'a list -> 'a list * 'a list =
 fun xs ->
  let n = List.length xs in
  go (n / 2) xs []

and go : int -> 'a list -> 'a list -> 'a list * 'a list =
 fun n xs ys ->
  match (n, xs, ys) with
  | 0, xs, ys -> (xs, ys)
  | n, x :: xs, ys -> go (n - 1) xs (x :: ys)
  | _, [], ys -> ([], ys)

let rec run : debug:int -> surface_expr -> expr =
 fun ~debug e ->
  let e' = go e in
  if debug >= 3 then traceln "[pass] shrink\n%s" ([%derive.show: expr] e');
  e'

and go : surface_expr -> expr = function
  | `Int i -> `Int i
  | `Var v -> `Var v
  | `Bool b -> `Bool b
  | `Prim (EQ, [ a; b ]) -> `Binary (EQ, go a, go b)
  | `Prim (LT, [ a; b ]) -> `Binary (LT, go a, go b)
  | `Prim (LE, [ a; b ]) -> `Binary (LE, go a, go b)
  | `Prim (Not, [ a ]) -> `Unary (Not, go a)
  | `Prim (Add, [ a; b ]) -> `Binary (Add, go a, go b)
  | `Prim (Add, [ a; b; c ]) -> `Binary (Add, go a, `Binary (Add, go b, go c))
  | `Prim (Add, es) ->
      let es1, es2 = split es in
      `Binary (Add, `Prim (Add, es1) |> go, `Prim (Add, es2) |> go)
  | `Prim (And, [ a; b ]) -> `Binary (And, go a, go b)
  | `Prim (And, [ a; b; c ]) -> `Binary (And, go a, `Binary (And, go b, go c))
  | `Prim (And, es) ->
      let es1, es2 = split es in
      `Binary (And, `Prim (And, es1) |> go, `Prim (And, es2) |> go)
  | `Prim (Or, [ a; b ]) -> `Binary (Or, go a, go b)
  | `Prim (Or, [ a; b; c ]) -> `Binary (Or, go a, `Binary (Or, go b, go c))
  | `Prim (Or, es) ->
      let es1, es2 = split es in
      `Binary (Or, `Prim (Or, es1) |> go, `Prim (Or, es2) |> go)
  | `Prim (Sub, [ a; b ]) -> `Binary (Sub, go a, go b)
  | `Prim (Sub, a :: b :: es) -> `Prim (Sub, `Prim (Sub, [ a; b ]) :: es) |> go
  | `Let (x, t, e) -> `Let (x, go t, go e)
  | `If (c, t, f) -> `If (go c, go t, go f)
  | `Cond [ (`Var "else", e) ] -> go e
  | `Cond ((c, e) :: clauses) -> `If (go c, go e, go (`Cond clauses))
  | e -> BadExpr e |> raise
