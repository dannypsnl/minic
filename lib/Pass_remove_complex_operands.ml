open Ast
open Eio
open RcoExpr

let rec run : debug:int -> expr -> rco_expr =
 fun ~debug e ->
  let e = rco_expr e in
  if debug >= 2 then
    traceln "[pass] remove complex operands\n%s" (show_rco_expr e);
  e

and rco_atom : expr -> atom * (string * rco_expr) list =
 fun e ->
  match e with
  | `Var x -> (`Var x, [])
  | `Int i -> (`Int i, [])
  | `Bool b -> (`Bool b, [])
  | e ->
      let temp_binder = Variable.make "tmp" in
      let e' = rco_expr e in
      (`Var temp_binder, [ (temp_binder, e') ])

and rco_expr : expr -> rco_expr =
 fun e ->
  match e with
  | `Binary (op, l, r) ->
      let l', bindings = rco_atom l in
      let r', bindings' = rco_atom r in
      let bindings = bindings @ bindings' in
      bindings
      |> List.fold_left
           (fun body (x, e) -> `Let (x, e, body))
           (`Binary (op, l', r'))
  | `Unary (op, e) ->
      let e', bindings = rco_atom e in
      bindings
      |> List.fold_left (fun body (x, e) -> `Let (x, e, body)) (`Unary (op, e'))
  | `If (c, t, f) -> `If (rco_expr c, rco_expr t, rco_expr f)
  | `Let (x, t, body) -> `Let (x, rco_expr t, rco_expr body)
  | `Set (x, e) -> `Set (x, rco_expr e)
  | `Begin (es, e) -> `Begin (List.map rco_expr es, rco_expr e)
  | `While (cond, body) -> `While (rco_expr cond, rco_expr body)
  | `Var x -> `Var x
  | `Int i -> `Int i
  | `Bool b -> `Bool b
