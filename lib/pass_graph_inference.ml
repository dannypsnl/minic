open Ast
open Graph

let run : debug:int -> asm -> RegSet.t list -> Graph.t =
 fun ~debug prog live_sets ->
  let conflict_graph = ref Graph.empty in
  List.combine prog (List.tl live_sets)
  |> List.iter (fun (instr, live_after) ->
         let vs = RegSet.elements live_after in
         match instr with
         | Mov (d, s) ->
             vs
             |> List.iter (fun v ->
                    if
                      (not ([%derive.eq: reg] v d))
                      && not ([%derive.eq: src] (Reg.to_src v) s)
                    then
                      conflict_graph :=
                        !conflict_graph
                        |> Graph.overlay
                             (Graph.connect (Graph.vertex v) (Graph.vertex d))
                    else
                      conflict_graph :=
                        !conflict_graph |> Graph.overlay (Graph.vertex d))
         | Add (d, _, _) | Sub (d, _, _) ->
             print_endline "live set dododo";
             print_endline
               (Graph.connect (Graph.vertex (`Reg "test")) (Graph.vertex d)
               |> Graph.show);
             vs
             |> List.iter (fun v ->
                    if not ([%derive.eq: reg] v d) then
                      conflict_graph :=
                        !conflict_graph
                        |> Graph.overlay
                             (Graph.connect (Graph.vertex v) (Graph.vertex d))
                    else
                      conflict_graph :=
                        !conflict_graph |> Graph.overlay (Graph.vertex d))
         | Str _ | Ldr _ | Ret -> ());
  let g = !conflict_graph in
  if debug >= 1 then (
    print_endline "\n[pass] conflict graph";
    print_endline (Graph.show g));
  g
