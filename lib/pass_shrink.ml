open Ast
open Eio

exception BadExpr of expr

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

let rec run : debug:int -> expr -> expr =
 fun ~debug e ->
  let e' = go e in
  if debug >= 3 then traceln "[pass] shrink\n%s" ([%derive.show: expr] e');
  e'

and go : expr -> expr = function
  | `Int i -> `Int i
  | `Var v -> `Var v
  | `Bool b -> `Bool b
  | `Prim (Not, [ a ]) -> `Prim (Not, [ go a ])
  | `Prim (Add, [ a; b ]) -> `Prim (Add, [ go a; go b ])
  | `Prim (Add, [ a; b; c ]) ->
      `Prim (Add, [ go a; `Prim (Add, [ go b; go c ]) ])
  | `Prim (Add, es) ->
      let es1, es2 = split es in
      `Prim (Add, [ `Prim (Add, es1) |> go; `Prim (Add, es2) |> go ])
  | `Prim (And, [ a; b ]) -> `Prim (And, [ go a; go b ])
  | `Prim (And, [ a; b; c ]) ->
      `Prim (And, [ go a; `Prim (And, [ go b; go c ]) ])
  | `Prim (And, es) ->
      let es1, es2 = split es in
      `Prim (And, [ `Prim (And, es1) |> go; `Prim (And, es2) |> go ])
  | `Prim (Or, [ a; b ]) -> `Prim (Or, [ go a; go b ])
  | `Prim (Or, [ a; b; c ]) -> `Prim (Or, [ go a; `Prim (Or, [ go b; go c ]) ])
  | `Prim (Or, es) ->
      let es1, es2 = split es in
      `Prim (Or, [ `Prim (Or, es1) |> go; `Prim (Or, es2) |> go ])
  | `Prim (Sub, [ a; b ]) -> `Prim (Sub, [ go a; go b ])
  | `Prim (Sub, a :: b :: es) ->
      let a' = go a in
      let b' = go b in
      `Prim (Sub, `Prim (Sub, [ a'; b' ]) :: es) |> go
  | `Let (x, t, e) -> `Let (x, go t, go e)
  | `If (c, t, f) -> `If (go c, go t, go f)
  | e -> BadExpr e |> raise
