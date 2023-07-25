open Ast

let rec run : debug:int -> asm -> asm =
 fun ~debug prog ->
  let prog = go prog in
  if debug >= 1 then (
    print_endline "\nstage 8: patch stack operations";
    print_endline (show_asm prog));
  prog

(* TODO: patch source load from stack *)
and go : asm -> asm = function
  | [] -> []
  | Mov (d, s) :: prog ->
      let rw =
        match patch d with
        | Some p -> Mov (`Reg "x28", s) :: p
        | _ -> [ Mov (d, s) ]
      in
      List.concat [ rw; go prog ]
  | Add (d, s1, s2) :: prog ->
      let rw =
        match patch d with
        | Some p -> Add (`Reg "x28", s1, s2) :: p
        | _ -> [ Add (d, s1, s2) ]
      in
      List.concat [ rw; go prog ]
  | Sub (d, s1, s2) :: prog ->
      let rw =
        match patch d with
        | Some p -> Sub (`Reg "x28", s1, s2) :: p
        | _ -> [ Sub (d, s1, s2) ]
      in
      List.concat [ rw; go prog ]
  | Str (s, sp, i) :: prog -> Str (s, sp, i) :: prog
  | Ldr (d, sp, i) :: prog -> Ldr (d, sp, i) :: prog
  | Ret :: prog -> Ret :: prog

and patch : reg -> asm option = function
  | `Reg _ -> None
  | `Sp i -> Some [ Str (`Reg "x28", `Reg "x31", i) ]
  | _ -> None
