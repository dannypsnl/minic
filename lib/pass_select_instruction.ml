open Ast
open Eio

let rec run : debug:int -> basic_blocks -> asm =
 fun ~debug bb ->
  let prog =
    bb
    |> List.map (fun (label, { name; body = tail }) ->
           let s = ref None in
           let instrs = go s tail in
           (label, { name; instrs; successor = !s }))
  in
  if debug >= 2 then traceln "[pass] select instructions\n%s" (show_asm prog);
  prog

and go : label option ref -> ctail -> instruction list =
 fun successor t ->
  match t with
  | Return e -> compile_assign e (`Reg "x0") @ [ Ret ]
  | Seq (Assign (x, e), c) -> compile_assign e (`Var x) @ go successor c
  | Goto label ->
      successor := Some label;
      [ B label ]
  | If { cmp = `Eq; a = `CVar x; b = `CInt 1; thn; els } ->
      [ CBNZ (`Var x, thn); B els ]
  | If { cmp = `Eq; a = `CInt i; b = `CInt 1; thn; els } ->
      if i = 1 then [ B thn ] else [ B els ]
  | If { cmp = `Eq; a = `CVar x; b = `CInt 0; thn; els } ->
      [ CBZ (`Var x, thn); B els ]
  | If { cmp = `Eq; a = `CInt i; b = `CInt 0; thn; els } ->
      if i = 0 then [ B thn ] else [ B els ]
  | If { cmp = `And; a = `CInt a; b = `CInt b; thn; els } ->
      if a = 1 && b = 1 then [ B thn ] else [ B els ]
  | If { cmp = `And; a = `CVar x; b = e2; thn; els } ->
      [ iand (`Var x, Reg.to_src (`Var x), compile_atom e2) ]
      @ [ CBNZ (`Var x, thn); B els ]
  | If _ ->
      traceln "%s" (show_ctail t);
      failwith "TODO"
  | _ -> failwith "TODO"

and compile_assign : cexpr -> dest -> instruction list =
 fun expression assign_to ->
  match expression with
  | `CInt i -> [ Mov (assign_to, `Imm i) ]
  | `CVar x -> [ Mov (assign_to, `Var x) ]
  | `Not e ->
      [
        Mov (assign_to, compile_atom e);
        Xor (assign_to, Reg.to_src assign_to, `Imm 1);
      ]
  | `Add (e, `CVar x) | `Add (`CVar x, e) -> single_expr add assign_to x e
  | `Sub (`CVar x, e) -> single_expr sub assign_to x e
  | `Add (e1, e2) -> two_expr add assign_to e1 e2
  | `Sub (e1, e2) -> two_expr sub assign_to e1 e2
  | `And (e1, e2) -> two_expr iand assign_to e1 e2
  | `Or (e1, e2) -> two_expr ior assign_to e1 e2
  | `EQ (e1, e2) -> compare assign_to e1 e2 `Eq
  | `LT (e1, e2) -> compare assign_to e1 e2 `Lt
  | `LE (e1, e2) -> compare assign_to e1 e2 `Le
  | _ -> failwith "TODO"

and compare assign_to e1 e2 cond =
  let pre, e1' =
    match e1 with
    | `CInt i -> ([ Mov (assign_to, `Imm i) ], assign_to)
    | `CVar x -> ([], `Var x)
  in
  let tmp1 = `Var (Variable.make "tmp") in
  let tmp2 = `Var (Variable.make "tmp") in
  pre
  @ [
      Cmp (e1', compile_atom e2);
      Mov (tmp1, `Imm 1);
      Mov (tmp2, `Imm 0);
      Csel (assign_to, tmp1, tmp2, cond);
    ]

and single_expr :
    (reg * src * src -> instruction) ->
    dest ->
    string ->
    catom ->
    instruction list =
 fun c assign_to x e -> [ c (assign_to, `Var x, compile_atom e) ]

and two_expr :
    (reg * src * src -> instruction) ->
    dest ->
    catom ->
    catom ->
    instruction list =
 fun c assign_to e1 e2 ->
  [
    Mov (assign_to, compile_atom e1);
    c (assign_to, Reg.to_src assign_to, compile_atom e2);
  ]

and compile_atom : catom -> src = function
  | `CInt i -> `Imm i
  | `CVar x -> `Var x
