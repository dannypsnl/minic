open Ast
open Graph
open Eio

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
         | Add (d, _, _)
         | Sub (d, _, _)
         | Xor (d, _, _)
         | Or (d, _, _)
         | And (d, _, _) ->
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
  if debug >= 1 then traceln "[pass] conflict graph\n%s" (Graph.show g);
  g
