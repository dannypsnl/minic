open Ast

type vertex = V of { value : reg; adjacency : RegSet.t }

module Vertex = struct
  type t = vertex

  let compare (V { value = a; _ }) (V { value = b; _ }) = [%derive.ord: reg] a b
  let show (V { value = reg; _ }) = show_reg reg
end

module VertexSet = Set.Make (Vertex)

type graph = VertexSet.t

module Graph = struct
  type t = graph

  let empty : t = VertexSet.empty

  let vertex reg =
    VertexSet.singleton (V { value = reg; adjacency = RegSet.empty })

  let overlay : t -> t -> t = fun g1 g2 -> VertexSet.union g1 g2

  let connect : t -> t -> t =
   fun g1 g2 ->
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

  let verticies : t -> vertex list = fun g -> VertexSet.to_seq g |> List.of_seq

  let edges : t -> (Reg.t * Reg.t) list =
   fun g ->
    VertexSet.to_seq g
    |> Seq.map (function V { value = v; adjacency = s } ->
           RegSet.to_seq s |> Seq.map (fun r -> (v, r)))
    |> Seq.concat |> List.of_seq

  let show : t -> string =
   fun g ->
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
    ^ " }\n"
    ^ (paths |> List.concat |> String.concat " ")
end

let%test "graph basic connect" =
  let g = Graph.vertex (`Var "a") |> Graph.connect (Graph.vertex (`Var "b")) in
  g |> Graph.verticies |> List.length = 2 && g |> Graph.edges |> List.length = 1

let%test "graph connect two vertices at once" =
  let g =
    Graph.vertex (`Var "a")
    |> Graph.connect
         (Graph.overlay (Graph.vertex (`Var "b")) (Graph.vertex (`Var "c")))
  in
  g |> Graph.verticies |> List.length = 3 && g |> Graph.edges |> List.length = 2
