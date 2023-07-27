open Ast

let rec run : debug:int -> expr -> rco_expr =
 fun ~debug e ->
  temp_var_cnt := 1;
  let e = rco_expr e in
  if debug >= 2 then (
    print_endline "\n[pass] remove complex operands";
    print_endline (show_rco_expr e));
  e

and rco_atom : expr -> atom * (string * rco_expr) list =
 fun e ->
  match e with
  | `Var x -> (`Var x, [])
  | `Int i -> (`Int i, [])
  | `Bool b -> (`Bool b, [])
  | e ->
      let temp_binder = "tmp." ^ Int.to_string !temp_var_cnt in
      temp_var_cnt := !temp_var_cnt + 1;
      let e' = rco_expr e in
      (`Var temp_binder, [ (temp_binder, e') ])

and rco_expr : expr -> rco_expr =
 fun e ->
  match e with
  | `Prim (op, es) ->
      let es' = List.map rco_atom es in
      let atoms, bb = List.split es' in
      let bindings = List.concat bb in
      bindings
      |> List.fold_left
           (fun body (x, e) -> `Let (x, e, body))
           (`Prim (op, atoms))
  | `Let (x, t, body) -> `Let (x, rco_expr t, rco_expr body)
  | `Var x -> `Var x
  | `Int i -> `Int i
  | `Bool b -> `Bool b

and temp_var_cnt = ref 1
