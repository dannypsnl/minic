open Ast

let add (d, s1, s2) = Add (d, s1, s2)
and sub (d, s1, s2) = Sub (d, s1, s2)
and xor (d, s1, s2) = Xor (d, s1, s2)

let rec run : debug:int -> asm -> asm =
 fun ~debug prog ->
  let prog = prog |> List.map patch_instruction |> List.concat in
  if debug >= 1 then (
    print_endline "\n[pass] patch stack operations";
    print_endline (show_asm prog));
  prog

and patch_instruction : instruction -> asm = function
  | Mov (`Sp i, s) ->
      List.append (patch_instruction (Mov (`Reg "x28", s))) (patch_dest (`Sp i))
  | Mov (d, `Sp i) ->
      List.append
        (patch_src (`Reg "x28") (`Sp i))
        (patch_instruction (Mov (d, `Reg "x28")))
  | Mov (d, s) -> [ Mov (d, s) ]
  | Add (`Sp i, s1, s2) -> f add i s1 s2
  | Add (d, `Sp i, s2) -> f2 add d i s2
  | Add (d, s1, `Sp i) -> f3 add d s1 i
  | Add (d, s1, s2) -> [ Add (d, s1, s2) ]
  | Sub (`Sp i, s1, s2) -> f sub i s1 s2
  | Sub (d, `Sp i, s2) -> f2 sub d i s2
  | Sub (d, s1, `Sp i) -> f3 sub d s1 i
  | Sub (d, s1, s2) -> [ Sub (d, s1, s2) ]
  | Xor (`Sp i, s1, s2) -> f xor i s1 s2
  | Xor (d, `Sp i, s2) -> f2 xor d i s2
  | Xor (d, s1, `Sp i) -> f3 xor d s1 i
  | Xor (d, s1, s2) -> [ Xor (d, s1, s2) ]
  | i -> [ i ]

and f : (reg * src * src -> instruction) -> int -> src -> src -> asm =
 fun c i s1 s2 ->
  List.append (patch_instruction (c (`Reg "x28", s1, s2))) (patch_dest (`Sp i))

and f2 : (reg * src * src -> instruction) -> reg -> int -> src -> asm =
 fun c d i s2 ->
  List.append
    (patch_src (`Reg "x28") (`Sp i))
    (patch_instruction (c (d, `Reg "x28", s2)))

and f3 : (reg * src * src -> instruction) -> reg -> src -> int -> asm =
 fun c d s1 i ->
  List.append
    (patch_src (`Reg "x27") (`Sp i))
    (patch_instruction (c (d, s1, `Reg "x27")))

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
