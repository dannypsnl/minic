open Ast

let rec run : debug:int -> asm -> asm =
 fun ~debug prog ->
  let prog = go prog in
  if debug >= 1 then (
    print_endline "\nstage 8: patch stack operations";
    print_endline (show_asm prog));
  prog

and go : asm -> asm = function
  | [] -> []
  | i :: prog -> List.append (patch_instruction i) (go prog)

and patch_instruction : instruction -> asm = function
  | Mov (`Sp i, s) ->
      List.append (patch_instruction (Mov (`Reg "x28", s))) (patch_dest (`Sp i))
  | Mov (d, `Sp i) ->
      List.append
        (patch_src (`Reg "x28") (`Sp i))
        (patch_instruction (Mov (d, `Reg "x28")))
  | Mov (d, s) -> [ Mov (d, s) ]
  | Add (`Sp i, s1, s2) ->
      List.append
        (patch_instruction (Add (`Reg "x28", s1, s2)))
        (patch_dest (`Sp i))
  | Add (d, `Sp i, s2) ->
      List.append
        (patch_src (`Reg "x28") (`Sp i))
        (patch_instruction (Add (d, `Reg "x28", s2)))
  | Add (d, s1, `Sp i) ->
      List.append
        (patch_src (`Reg "x27") (`Sp i))
        (patch_instruction (Add (d, s1, `Reg "x27")))
  | Add (d, s1, s2) -> [ Add (d, s1, s2) ]
  | Sub (`Sp i, s1, s2) ->
      List.append
        (patch_instruction (Sub (`Reg "x28", s1, s2)))
        (patch_dest (`Sp i))
  | Sub (d, `Sp i, s2) ->
      List.append
        (patch_src (`Reg "x28") (`Sp i))
        (patch_instruction (Sub (d, `Reg "x28", s2)))
  | Sub (d, s1, `Sp i) ->
      List.append
        (patch_src (`Reg "x27") (`Sp i))
        (patch_instruction (Sub (d, s1, `Reg "x27")))
  | Sub (d, s1, s2) -> [ Sub (d, s1, s2) ]
  | i -> [ i ]

and patch_dest : reg -> asm = function
  (* store x28 to stack *)
  | `Sp i -> [ Str (`Reg "x28", `Reg "sp", i) ]
  | _ -> []

and patch_src : reg -> src -> asm =
 fun load_to src ->
  match src with
  (* load stack into x28 *)
  | `Sp i -> [ Ldr (load_to, `Reg "sp", i) ]
  | _ -> []
