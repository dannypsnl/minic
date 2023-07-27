open Ast

exception TODO

let rec run : debug:int -> ctail -> asm =
 fun ~debug t ->
  let prog = go t in
  if debug >= 2 then (
    print_endline "\n[pass] select instructions";
    print_endline (show_asm prog));
  prog

and go : ctail -> asm = function
  | Return e -> compile_assign e (`Reg "x0") @ [ Ret ]
  | Seq (Assign (x, e), c) -> compile_assign e (`Var x) @ go c

and compile_assign : cexpr -> dest -> asm =
 fun expression assign_to ->
  match expression with
  | `CInt i -> [ Mov (assign_to, `Imm i) ]
  | `CVar x -> [ Mov (assign_to, `Var x) ]
  | `Not e ->
      [
        Mov (assign_to, compile_atom e);
        Xor (assign_to, Reg.to_src assign_to, `Imm 1);
      ]
  | `Add (e, `CVar x) | `Add (`CVar x, e) ->
      [ Add (assign_to, `Var x, compile_atom e) ]
  | `Sub (`CVar x, e) -> [ Sub (assign_to, `Var x, compile_atom e) ]
  | `Add (e1, e2) ->
      [
        Mov (assign_to, compile_atom e1);
        Add (assign_to, Reg.to_src assign_to, compile_atom e2);
      ]
  | `Sub (e1, e2) ->
      [
        Mov (assign_to, compile_atom e1);
        Sub (assign_to, Reg.to_src assign_to, compile_atom e2);
      ]

and compile_atom : catom -> src = function
  | `CInt i -> `Imm i
  | `CVar x -> `Var x
