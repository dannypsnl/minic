open Ast
open Eio

let rec run : debug:int -> asm -> asm =
 fun ~debug prog ->
  let prog = prog |> List.map patch_instruction |> List.concat in
  if debug >= 2 then traceln "[pass] move biasing\n%s" (show_asm prog);
  prog

and patch_instruction : instruction -> asm = function
  | Mov (d, s) ->
      if [%derive.eq: src] (Reg.to_src d) s then [] else [ Mov (d, s) ]
  | i -> [ i ]
