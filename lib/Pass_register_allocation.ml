open Ast
open Graph
open Eio
open AArch64

exception NotFound of string
exception ShouldFilterOut

module ColorM = Hashtbl.Make (String)

let rec run : debug:int -> asm -> (label * Graph.t) list -> asm =
 fun ~debug prog block_graphs ->
  let color_map : reg ColorM.t = ColorM.create 100 in
  let prog =
    prog
    |> List.map (fun (label, { name; instrs; successor }) ->
           ( label,
             {
               name;
               instrs =
                 block_allocate color_map instrs
                   (List.assoc label block_graphs |> Graph.verticies);
               successor;
             } ))
  in
  if debug >= 2 then traceln "[pass] register allocation\n%s" (show_asm prog);
  prog

and block_allocate :
    reg ColorM.t -> instruction list -> vertex list -> instruction list =
 fun color_map prog working_set ->
  let working_set : vertex list =
    working_set
    (* filter out register, they should not be rewritten by this pass *)
    |> List.filter (fun { value; _ } ->
           match value with `Var _ -> true | _ -> false)
    |> List.sort (fun { adjacency = s; _ } { adjacency = s2; _ } ->
           let a = RegSet.elements s |> List.length in
           let b = RegSet.elements s2 |> List.length in
           if a = b then 0 else if a < b then -1 else 1)
  in
  coloring working_set color_map;
  let drw = subst_dest color_map in
  let srw = subst_src color_map in
  List.map
    (function
      | Add (d, s1, s2) -> Add (drw d, srw s1, srw s2)
      | Sub (d, s1, s2) -> Sub (drw d, srw s1, srw s2)
      | Xor (d, s1, s2) -> Xor (drw d, srw s1, srw s2)
      | Or (d, s1, s2) -> Or (drw d, srw s1, srw s2)
      | And (d, s1, s2) -> And (drw d, srw s1, srw s2)
      | Mov (d, s) -> Mov (drw d, srw s)
      | CBNZ (c, label) -> CBNZ (drw c, label)
      | CBZ (c, label) -> CBZ (drw c, label)
      | B label -> B label
      | Cmp (r, s) -> Cmp (drw r, srw s)
      | Csel (d, r1, r2, c) -> Csel (drw d, drw r1, drw r2, c)
      | instr -> instr)
    prog

and subst_dest : reg ColorM.t -> dest -> dest =
 fun colors -> function
  | `Var x -> (
      match ColorM.find_opt colors x with Some v -> v | None -> `Reg "x26")
  | reg -> reg

and subst_src : reg ColorM.t -> src -> src =
 fun colors s ->
  match s with
  | `Var x ->
      (match ColorM.find_opt colors x with Some v -> v | None -> `Reg "x26")
      |> Reg.to_src
  | `Imm i -> `Imm i
  | reg -> reg

and coloring : vertex list -> reg ColorM.t -> unit =
 fun ws colors ->
  match ws with
  | [] -> ()
  | { value = `Var x; adjacency = vs } :: rest ->
      let ad =
        RegSet.map
          (function
            | `Var x -> (
                match ColorM.find_opt colors x with
                | Some r -> r
                | None -> `Var x)
            | reg -> reg)
          vs
      in
      let available = RegSet.diff all_register ad in

      let sp_list =
        RegSet.elements ad
        |> List.filter (function
             | `Reg _ -> false
             | `Var _ -> false
             | `Sp _ -> true)
        |> List.sort (fun a b ->
               match (a, b) with `Sp i, `Sp j -> compare i j | _ -> 0)
      in
      let max_shift = match sp_list with `Sp i :: _ -> i | _ :: _ | [] -> 0 in
      let picked_color =
        match RegSet.elements available with
        | [] -> `Sp (max_shift + 8)
        | h :: _ -> h
      in
      ColorM.add colors x picked_color;
      coloring rest colors
  | { value = _; _ } :: _ -> raise ShouldFilterOut

and all_register : RegSet.t =
  RegSet.of_list
    [
      (* function call might take x0-x8 *)
      (* `Reg "x0"; *)
      `Reg "x1";
      `Reg "x2";
      `Reg "x3";
      `Reg "x4";
      `Reg "x5";
      `Reg "x6";
      `Reg "x7";
      `Reg "x8";
      (* middle x9-x26 is safe to use *)
      `Reg "x9";
      `Reg "x10";
      `Reg "x11";
      `Reg "x12";
      `Reg "x13";
      `Reg "x14";
      `Reg "x15";
      `Reg "x16";
      `Reg "x17";
      `Reg "x18";
      `Reg "x19";
      `Reg "x20";
      `Reg "x21";
      `Reg "x22";
      `Reg "x23";
      `Reg "x24";
      `Reg "x25";
      `Reg "x26";
      (* `x27` and `x28` are taken for stack operations *)
      (* `x29` is frame pointer *)
      (* `x30` stores the return address of function *)
      (* `x31` is stack pointer, alias is `sp` *)
    ]
