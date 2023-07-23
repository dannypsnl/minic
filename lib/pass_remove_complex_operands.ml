open Ast

let rec run : expr -> expr =
 fun e ->
  let temp_var_cnt = ref 1 in
  rco_expr temp_var_cnt e

and rco_atom : int ref -> expr -> expr * (string * expr) list =
 fun temp_var_cnt e ->
  match e with
  | Var x -> (Var x, [])
  | Int i -> (Int i, [])
  | Prim (op, es) ->
      let es' = List.map (rco_atom temp_var_cnt) es in
      let atoms, bb = List.split es' in
      let temp_binder = "tmp." ^ Int.to_string !temp_var_cnt in
      temp_var_cnt := !temp_var_cnt + 1;
      (Var temp_binder, (temp_binder, Prim (op, atoms)) :: List.concat bb)
  | Let (x, t, body) ->
      (Let (x, rco_expr temp_var_cnt t, rco_expr temp_var_cnt body), [])

and rco_expr : int ref -> expr -> expr =
 fun temp_var_cnt e ->
  match e with
  | Var _ | Int _ -> e
  | Prim (op, es) ->
      let es' = List.map (rco_atom temp_var_cnt) es in
      let atoms, bb = List.split es' in
      let bindings = List.concat bb in
      List.fold_right
        (fun (x, e) body -> Let (x, e, body))
        bindings
        (Prim (op, atoms))
  | Let (x, t, body) ->
      Let (x, rco_expr temp_var_cnt t, rco_expr temp_var_cnt body)
