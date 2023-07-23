open Ast

exception TODO

let rec run : ctail -> asm = function
  | Return e -> compile_assign e (`Reg "x0") @ [ Ret ]
  | Seq (Assign (x, e), c) -> compile_assign e (`Var x) @ run c

and compile_assign : cexpr -> dest -> asm =
 fun expression assign_to ->
  match expression with
  | `CInt i -> [ Mov (assign_to, `Imm i) ]
  | `CVar x -> [ Mov (assign_to, `Var x) ]
  | `CPrim (op, [ e1; e2 ]) -> (
      match op with
      | Add -> [ Add (assign_to, compile_atom e1, compile_atom e2) ]
      | Sub -> [ Sub (assign_to, compile_atom e1, compile_atom e2) ])
  | `CPrim (_, es) -> raise (ToManyArguments (List.length es))

and compile_atom : catom -> src = function
  | `CInt i -> `Imm i
  | `CVar x -> `Var x
