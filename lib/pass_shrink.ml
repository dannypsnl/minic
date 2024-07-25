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
  | `UPrim (op, a) -> `Unary (op, go a)
  | `Prim (op, a, b) -> `Binary (op, go a, go b)
  | `Let (x, t, e) -> `Let (x, go t, go e)
  | `If (c, t, f) -> `If (go c, go t, go f)
  | `Set (x, e) -> `Set (x, go e)
  | `Begin es -> (
      match List.rev es with
      | [] -> BadExpr (`Begin []) |> raise
      | e :: es -> `Begin (List.map go (List.rev es), go e))
  | `While (c, b) -> `While (go c, go b)
