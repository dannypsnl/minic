open Ast
open Graph

let run : debug:bool -> asm -> RegSet.t list -> Graph.t =
 fun ~debug prog live_sets ->
  let conflict_graph = ref Graph.empty in
  List.combine prog (List.tl live_sets)
  |> List.iter (fun (instr, live_after) ->
         let vs = RegSet.elements live_after in
         match instr with
         | Mov (d, s) ->
             List.iter
               (fun v ->
                 if
                   (not ([%derive.eq: reg] v d))
                   && not ([%derive.eq: src] (Reg.to_src v) s)
                 then
                   conflict_graph :=
                     Graph.connect (Graph.vertex v) (Graph.vertex d)
                     |> Graph.overlay !conflict_graph
                 else
                   conflict_graph :=
                     Graph.vertex d |> Graph.overlay !conflict_graph)
               vs
         | Add (d, _, _) | Sub (d, _, _) ->
             List.iter
               (fun v ->
                 if not ([%derive.eq: reg] v d) then
                   conflict_graph :=
                     Graph.connect (Graph.vertex v) (Graph.vertex d)
                     |> Graph.overlay !conflict_graph
                 else
                   conflict_graph :=
                     Graph.vertex d |> Graph.overlay !conflict_graph)
               vs
         | Ret -> ());
  let g = !conflict_graph in
  if debug then (
    print_endline "\nstage 6: conflict graph";
    print_endline (Graph.show g));
  g
