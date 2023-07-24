open Ast
open Graph

exception FilterOut

module ColorM = Hashtbl.Make (String)

let rec run : debug:int -> asm -> Graph.t -> asm =
 fun ~debug prog g ->
  let working_set = Graph.verticies g in
  let working_set : vertex list =
    working_set
    (* filter out register, they should not be rewritten by this pass *)
    |> List.filter (fun (V { value = v; _ }) ->
           match v with `Reg _ -> false | `Var _ -> true)
    |> List.sort (fun (V { adjacency = s; _ }) (V { adjacency = s2; _ }) ->
           let a = RegSet.elements s |> List.length in
           let b = RegSet.elements s2 |> List.length in
           if a = b then 0 else if a < b then -1 else 1)
  in
  let color_map : reg ColorM.t = ColorM.create (List.length working_set) in
  coloring working_set color_map;
  let drw = subst_dest color_map in
  let srw = subst_src color_map in
  let prog =
    List.map
      (function
        | Add (d, s1, s2) -> Add (drw d, srw s1, srw s2)
        | Sub (d, s1, s2) -> Sub (drw d, srw s1, srw s2)
        | Mov (d, s) -> Mov (drw d, srw s)
        | Ret -> Ret)
      prog
  in
  if debug >= 1 then (
    print_endline "\nstage 7: register allocation";
    print_endline (show_asm prog));
  prog

and subst_dest : reg ColorM.t -> dest -> dest =
 fun colors d ->
  match d with `Reg r -> `Reg r | `Var x -> ColorM.find colors x

and subst_src : reg ColorM.t -> src -> src =
 fun colors s ->
  match s with
  | `Reg r -> `Reg r
  | `Var x -> ColorM.find colors x |> Reg.to_src
  | `Imm i -> `Imm i

and coloring : vertex list -> reg ColorM.t -> unit =
 fun ws colors ->
  match ws with
  | [] -> ()
  | V { value = `Var x; adjacency = vs } :: rest ->
      let ad =
        RegSet.map
          (function
            | `Reg x -> `Reg x
            | `Var x -> (
                match ColorM.find_opt colors x with
                | Some r -> r
                | None -> `Var x))
          vs
      in
      let available = RegSet.diff all_register ad in
      let picked_color = RegSet.elements available |> List.hd in
      ColorM.add colors x picked_color;
      coloring rest colors
  | V { value = _; _ } :: _ -> raise FilterOut

and all_register : RegSet.t =
  RegSet.of_list
    [
      `Reg "x0";
      `Reg "x1";
      `Reg "x2";
      `Reg "x3";
      `Reg "x4";
      `Reg "x5";
      `Reg "x6";
      `Reg "x7";
      `Reg "x8";
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
    ]
