open Ast

type vertex =
  | V of {
      value : reg; (* each register is a vertex of conflict graph *)
      adjacency : RegSet.t; (* connected register *)
    }

module Vertex = struct
  type t = vertex

  let compare : vertex -> vertex -> int =
   fun (V { value = a; _ }) (V { value = b; _ }) -> [%derive.ord: reg] a b

  let show (V { value = reg; _ }) = show_reg reg
end

module VertexSet = Set.Make (Vertex)

type graph = VertexSet.t

module Graph = struct
  type t = graph

  let empty = VertexSet.empty

  let vertex reg =
    VertexSet.singleton (V { value = reg; adjacency = RegSet.empty })

  let overlay g1 g2 = VertexSet.union g1 g2

  let connect g1 g2 =
    VertexSet.map
      (function
        | V { value = v; adjacency = s } ->
            let g2_vertcies =
              VertexSet.to_seq g2 |> List.of_seq
              |> List.map (function V { value = r; _ } -> r)
              |> RegSet.of_list
            in
            V { value = v; adjacency = s |> RegSet.union g2_vertcies })
      g1
    |> VertexSet.union g2

  let show g =
    let vs = VertexSet.to_seq g |> List.of_seq in
    let paths =
      vs
      |> List.map (fun (V { value = from; adjacency = s }) ->
             RegSet.to_seq s |> List.of_seq
             |> List.map (fun to_ ->
                    Printf.sprintf "%s -> %s" (show_reg from) (show_reg to_)))
    in
    "{ "
    ^ (vs |> List.map Vertex.show |> String.concat ", ")
    ^ " }" ^ "\n"
    ^ (paths |> List.concat |> String.concat " ")
end
