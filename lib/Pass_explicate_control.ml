open Ast
open Eio
open RcoExpr

let rec run ~(debug : int) (e : rco_expr) : basic_blocks =
  let bb = ref [] in
  let r = explicate_tail ~bb e in
  let bb = ("entry", { name = "entry"; body = r }) :: !bb in
  if debug >= 2 then
    traceln "[pass] explicate control\n%s" (show_basic_blocks bb);
  bb

and explicate_tail ~(bb : basic_blocks ref) (e : rco_expr) : ctail =
  match e with
  | `Bool true -> Return (`CInt 1)
  | `Bool false -> Return (`CInt 0)
  | `Int i -> Return (`CInt i)
  | `Var x -> Return (`CVar x)
  | `Let (x, t, body) -> explicate_assign ~bb t x (explicate_tail ~bb body)
  | `If (condition, thn, els) ->
      explicate_pred ~bb condition (explicate_tail ~bb thn)
        (explicate_tail ~bb els)
  | `Set (x, e) -> explicate_assign ~bb e x (Return `Void)
  | `Begin (es, e) ->
      List.fold_right (explicate_effect ~bb) es (explicate_tail ~bb e)
  | `While _ -> explicate_effect ~bb e (Return `Void)
  | `Unary (Not, a) -> Return (`Not (explicate_atom a))
  | `Binary (Add, a, b) -> Return (`Add (explicate_atom a, explicate_atom b))
  | `Binary (Sub, a, b) -> Return (`Sub (explicate_atom a, explicate_atom b))
  | `Binary (And, a, b) -> Return (`And (explicate_atom a, explicate_atom b))
  | `Binary (Or, a, b) -> Return (`Or (explicate_atom a, explicate_atom b))
  | `Binary (EQ, a, b) -> Return (`EQ (explicate_atom a, explicate_atom b))
  | `Binary (LT, a, b) -> Return (`LT (explicate_atom a, explicate_atom b))
  | `Binary (LE, a, b) -> Return (`LE (explicate_atom a, explicate_atom b))

(* In this function, we assign `e` to `x`, then do `cont` computation.
 *)
and explicate_assign ~(bb : basic_blocks ref) (e : rco_expr) (x : string)
    (cont : ctail) : ctail =
  match e with
  | `Bool true -> Seq (Assign (x, `CInt 1), cont)
  | `Bool false -> Seq (Assign (x, `CInt 0), cont)
  | `Int i -> Seq (Assign (x, `CInt i), cont)
  | `Var v -> Seq (Assign (x, `CVar v), cont)
  | `Begin (es, e) ->
      List.fold_right (explicate_effect ~bb) es (explicate_assign ~bb e x cont)
  | `Let (x2, t, body) ->
      let body' = explicate_assign ~bb body x cont in
      explicate_assign ~bb t x2 body'
  | `If (c, t, f) ->
      (* Consider code like

          let x := if c then 1 else 2 in
          x + 10

         This code shouldn't be transformed to

          if c then
            let x := 1 in x + 10
          else
            let x := 2 in x + 10

         which duplicate the code `x + 10` twice. Instead, we should create a new block

          if c then
            x := 1; goto L0
          else
            x := 2; goto L0;
          L0:
            x + 10;
      *)
      let cont' = Goto (create_block ~bb cont) in
      explicate_pred ~bb c
        (explicate_assign ~bb t x cont')
        (explicate_assign ~bb f x cont')
  | `Unary (Not, a) -> Seq (Assign (x, `Not (explicate_atom a)), cont)
  | `Binary (Add, a, b) ->
      Seq (Assign (x, `Add (explicate_atom a, explicate_atom b)), cont)
  | `Binary (Sub, a, b) ->
      Seq (Assign (x, `Sub (explicate_atom a, explicate_atom b)), cont)
  | `Binary (And, a, b) ->
      Seq (Assign (x, `And (explicate_atom a, explicate_atom b)), cont)
  | `Binary (Or, a, b) ->
      Seq (Assign (x, `Or (explicate_atom a, explicate_atom b)), cont)
  | `Binary (EQ, a, b) ->
      Seq (Assign (x, `EQ (explicate_atom a, explicate_atom b)), cont)
  | `Binary (LT, a, b) ->
      Seq (Assign (x, `LT (explicate_atom a, explicate_atom b)), cont)
  | `Binary (LE, a, b) ->
      Seq (Assign (x, `LE (explicate_atom a, explicate_atom b)), cont)
  | _ ->
      (* while and set! are both invalid assign *)
      Reporter.fatalf Compile_error "explicate_assign unhandled case: %s"
      @@ show_rco_expr e

and explicate_pred ~(bb : basic_blocks ref) (pred : rco_expr) (thn : ctail)
    (els : ctail) : ctail =
  match pred with
  | `Var x -> cif ~bb `Eq (`CVar x) (`CInt 1) thn els
  | `Bool b -> if b then thn else els
  | `Let (x, t, body) ->
      explicate_assign ~bb t x @@ explicate_pred ~bb body thn els
  | `If (c, t, f) ->
      let name = Variable.make "tmp" in
      explicate_assign ~bb (`If (c, t, f)) name
      @@ explicate_pred ~bb (`Var name) thn els
  | `Unary (Not, a) -> cif ~bb `Eq (explicate_atom a) (`CInt 0) thn els
  | `Binary (And, a, b) ->
      cif ~bb `And (explicate_atom a) (explicate_atom b) thn els
  | `Binary (Or, a, b) ->
      cif ~bb `Or (explicate_atom a) (explicate_atom b) thn els
  | `Binary (EQ, a, b) ->
      cif ~bb `Eq (explicate_atom a) (explicate_atom b) thn els
  | `Binary (LT, a, b) ->
      cif ~bb `Lt (explicate_atom a) (explicate_atom b) thn els
  | `Binary (LE, a, b) ->
      cif ~bb `Le (explicate_atom a) (explicate_atom b) thn els
  | `While _ ->
      Reporter.fatalf Compile_error "use loop as condition is disallowed"
  (* TODO: need type checker to ensure this is impossible *)
  | _ ->
      Reporter.fatalf Compile_error "explicate_pred unhandled case: %s"
      @@ show_rco_expr pred

(* Evaluate `e` for its side effects, discard its value, then do `cont`. This is
   the statement position: `Begin` elements, loop bodies, etc. live here. *)
and explicate_effect ~(bb : basic_blocks ref) (e : rco_expr) (cont : ctail) :
    ctail =
  match e with
  | `Set (x, e) -> explicate_assign ~bb e x cont
  | `Begin (es, e) ->
      List.fold_right (explicate_effect ~bb) es (explicate_effect ~bb e cont)
  | `Let (x, t, body) ->
      explicate_assign ~bb t x (explicate_effect ~bb body cont)
  | `If (c, t, f) ->
      let cont' = Goto (create_block ~bb cont) in
      explicate_pred ~bb c
        (explicate_effect ~bb t cont')
        (explicate_effect ~bb f cont')
  | `While (condition, body) ->
      (* Reserve the loop label up front so the body can jump back to the test.
            loop:  if condition then goto <body> else goto <cont>
            <body>: ...; goto loop *)
      let loop_block = Variable.make "loop" in
      let test =
        explicate_pred ~bb condition
          (explicate_effect ~bb body (Goto loop_block))
          cont
      in
      bb := (loop_block, { name = loop_block; body = test }) :: !bb;
      Goto loop_block
  | _ ->
      (* a pure expression in statement position: its value is discarded, so it
         has no effect and we fall straight through to `cont`. *)
      cont

and cif ~(bb : basic_blocks ref) (op : cmp_op) (a : catom) (b : catom)
    (thn : ctail) (els : ctail) : ctail =
  If { cmp = op; a; b; thn = create_block ~bb thn; els = create_block ~bb els }

and create_block ~(bb : basic_blocks ref) (c : ctail) : label =
  match c with
  | Goto label -> label
  | tail ->
      let label = "block" ^ (!bb |> List.length |> Int.to_string) in
      bb := (label, { name = label; body = tail }) :: !bb;
      label

and explicate_atom (e : atom) : catom =
  match e with
  | `Bool true -> `CInt 1
  | `Bool false -> `CInt 0
  | `Int i -> `CInt i
  | `Var x -> `CVar x
