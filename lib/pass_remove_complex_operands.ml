open Ast

let rec run : debug:bool -> expr -> expr =
 fun ~debug e ->
  temp_var_cnt := 1;
  let e = rco_expr e in
  if debug then (
    print_endline "\nstage 2: remove complex operands";
    print_endline (show_expr e));
  e

and rco_atom : expr -> expr * (string * expr) list =
 fun e ->
  match e with
  | Var x -> (Var x, [])
  | Int i -> (Int i, [])
  | Prim (op, es) ->
      let es' = List.map rco_atom es in
      let atoms, bb = List.split es' in
      let temp_binder = "tmp." ^ Int.to_string !temp_var_cnt in
      temp_var_cnt := !temp_var_cnt + 1;
      (Var temp_binder, (temp_binder, Prim (op, atoms)) :: List.concat bb)
  | Let (x, t, body) ->
      let atom_body, bindings = rco_atom body in
      (atom_body, (x, rco_expr t) :: bindings)

and rco_expr : expr -> expr =
 fun e ->
  match e with
  | Var _ | Int _ -> e
  | Prim (op, es) ->
      let es' = List.map rco_atom es in
      let atoms, bb = List.split es' in
      let bindings = List.concat bb in
      List.fold_left
        (fun body (x, e) -> Let (x, e, body))
        (Prim (op, atoms))
        bindings
  | Let (x, t, body) -> Let (x, rco_expr t, rco_expr body)

and temp_var_cnt = ref 1
