open Ast
open Eio

exception ToManyArguments of int

let rec run : debug:int -> rco_expr -> basic_blocks =
 fun ~debug e ->
  temp_var_cnt := 1;
  let bb = ref [] in
  let r = explicate_tail ~bb e in
  if debug >= 2 then traceln "[pass] explicate control\n%s" (show_ctail r);
  ("entry", r) :: !bb

and explicate_tail : bb:basic_blocks ref -> rco_expr -> ctail =
 fun ~bb e ->
  match e with
  | `Bool true -> Return (`CInt 1)
  | `Bool false -> Return (`CInt 0)
  | `Int i -> Return (`CInt i)
  | `Var x -> Return (`CVar x)
  | `Let (x, t, body) -> explicate_assign ~bb t x (explicate_tail ~bb body)
  | `If (c, t, f) ->
      explicate_pred ~bb c (explicate_tail ~bb t) (explicate_tail ~bb f)
  | `Prim (Not, [ a ]) -> Return (`Not (explicate_atom a))
  | `Prim (Add, [ a; b ]) -> Return (`Add (explicate_atom a, explicate_atom b))
  | `Prim (Sub, [ a; b ]) -> Return (`Sub (explicate_atom a, explicate_atom b))
  | `Prim (And, [ a; b ]) -> Return (`And (explicate_atom a, explicate_atom b))
  | `Prim (Or, [ a; b ]) -> Return (`Or (explicate_atom a, explicate_atom b))
  | `Prim (_, es) -> raise (ToManyArguments (List.length es))

and explicate_assign :
    bb:basic_blocks ref -> rco_expr -> string -> ctail -> ctail =
 fun ~bb e x cont ->
  match e with
  | `Bool true -> Seq (Assign (x, `CInt 1), cont)
  | `Bool false -> Seq (Assign (x, `CInt 0), cont)
  | `Int i -> Seq (Assign (x, `CInt i), cont)
  | `Var x -> Seq (Assign (x, `CVar x), cont)
  | `Let (x2, t, body) ->
      let body' = explicate_assign ~bb body x cont in
      explicate_assign ~bb t x2 body'
  | `If (c, t, f) ->
      explicate_pred ~bb c (explicate_tail ~bb t) (explicate_tail ~bb f)
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

and explicate_pred : bb:basic_blocks ref -> rco_expr -> ctail -> ctail -> ctail
    =
 fun ~bb e thn els ->
  match e with
  | `Bool b -> if b then thn else els
  | `Var x ->
      If
        {
          cmp = `Eq;
          a = `CVar x;
          b = `CInt 1;
          thn = create_block ~bb thn;
          els = create_block ~bb els;
        }
  | `Let (x, t, body) ->
      explicate_assign ~bb t x @@ explicate_pred ~bb body thn els
  | `If (c, t, f) ->
      let name = "tmp2." ^ Int.to_string !temp_var_cnt in
      explicate_assign ~bb (`If (c, t, f)) name
      @@ explicate_pred ~bb (`Var name) thn els
  | `Prim (And, [ a; b ]) ->
      If
        {
          cmp = `And;
          a = explicate_atom a;
          b = explicate_atom b;
          thn = create_block ~bb thn;
          els = create_block ~bb els;
        }
  | `Prim (Or, [ a; b ]) ->
      If
        {
          cmp = `Or;
          a = explicate_atom a;
          b = explicate_atom b;
          thn = create_block ~bb thn;
          els = create_block ~bb els;
        }
  (* TODO: need type checker to ensure this is impossible *)
  | _ -> failwith @@ "explicate_pred unhandled case: " ^ show_rco_expr e

and create_block : bb:basic_blocks ref -> ctail -> label =
 fun ~bb c ->
  match c with
  | Goto label -> label
  | tail ->
      let label = "block" ^ (!bb |> List.length |> Int.to_string) in
      bb := (label, tail) :: !bb;
      label

and explicate_atom : atom -> catom =
 fun e ->
  match e with
  | `Bool true -> `CInt 1
  | `Bool false -> `CInt 0
  | `Int i -> `CInt i
  | `Var x -> `CVar x

and temp_var_cnt = ref 1
