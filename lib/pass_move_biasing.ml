open Ast

let rec run : debug:int -> asm -> asm =
 fun ~debug prog ->
  let prog = prog |> List.map patch_instruction |> List.concat in
  if debug >= 2 then (
    print_endline "\n[pass] move biasing";
    print_endline (show_asm prog));
  prog

and patch_instruction : instruction -> asm = function
  | Mov (d, s) ->
      if [%derive.eq: src] (Reg.to_src d) s then [] else [ Mov (d, s) ]
  | i -> [ i ]
