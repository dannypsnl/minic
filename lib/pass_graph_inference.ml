open Ast
open Graph
open Eio

let rec run :
    debug:int -> asm -> (label * RegSet.t list) list -> (label * Graph.t) list =
 fun ~debug prog block_live_sets ->
  let block_graphs =
    prog
    |> List.map (fun (label, instrs) ->
           (label, block_graph instrs (List.assoc label block_live_sets)))
  in
  if debug >= 1 then (
    traceln "[pass] conflict graph";
    List.iter
      (fun (label, graph) -> traceln "%s:\n%s" label (Graph.show graph))
      block_graphs);
  block_graphs

and block_graph : instruction list -> RegSet.t list -> Graph.t =
 fun instrs live_sets ->
  let conflict_graph = ref Graph.empty in
  List.combine instrs (List.tl live_sets)
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
         | Str _ | Ldr _ | Ret | B _ | CBZ _ | CBNZ _ -> ());
  !conflict_graph
