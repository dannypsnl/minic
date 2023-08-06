open Ast
open Eio

let rec run : debug:int -> Pass_explicate_control.basic_blocks -> asm =
 fun ~debug bb ->
  let prog = bb |> List.map (fun (label, block) -> (label, go block)) in
  if debug >= 2 then traceln "[pass] select instructions\n%s" (show_asm prog);
  prog

and go : ctail -> instruction list = function
  | Return e -> compile_assign e (`Reg "x0") @ [ Ret ]
  | Seq (Assign (x, e), c) -> compile_assign e (`Var x) @ go c
  | Goto label -> [ B label ]
  (* | If { cmp = `Eq; a; b = `CInt 1; thn; els } -> [] *)
  | If _ -> []

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
