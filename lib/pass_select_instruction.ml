open Ast

exception TODO
exception NotUnary of op
exception NotBinary of op

let rec run : debug:int -> ctail -> asm =
 fun ~debug t ->
  let prog = go t in
  if debug >= 2 then (
    print_endline "\nstage 4: select instructions";
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
  | `CUnary (op, _e) -> (
      match op with
      (*  *)
      | Not -> raise TODO
      | op -> raise (NotUnary op) (* test *))
  | `CBinary ((Add as op), e, `CVar x) | `CBinary (op, `CVar x, e) ->
      let c =
        match op with
        | Add -> fun (d, s1, s2) -> Add (d, s1, s2)
        | Sub -> fun (d, s1, s2) -> Sub (d, s1, s2)
        | op -> raise (NotBinary op)
      in

      [ c (assign_to, `Var x, compile_atom e) ]
  | `CBinary (op, e1, e2) -> (
      match op with
      | Add ->
          [
            Mov (assign_to, compile_atom e1);
            Add (assign_to, Reg.to_src assign_to, compile_atom e2);
          ]
      | Sub ->
          [
            Mov (assign_to, compile_atom e1);
            Sub (assign_to, Reg.to_src assign_to, compile_atom e2);
          ]
      | _ -> raise TODO)

and compile_atom : catom -> src = function
  | `CInt i -> `Imm i
  | `CVar x -> `Var x
