open Ast

exception TODO

type vertex =
  | V of {
      value : reg; (* each register is a vertex of conflict graph *)
      adjacency : RegSet.t; (* connected register *)
    }

module Vertex = struct
  type t = vertex

  let compare : vertex -> vertex -> int =
   fun (V { value = a; _ }) (V { value = b; _ }) -> [%derive.ord: reg] a b
end

module VertexSet = Set.Make (Vertex)

type graph = VertexSet.t

module Graph = struct
  type t = graph

  let empty = VertexSet.empty
  let vertex reg = V { value = reg; adjacency = RegSet.empty }
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
end
