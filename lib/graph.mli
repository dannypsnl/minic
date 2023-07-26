open Ast

type vertex = {
  value : reg; (* each register is a vertex of conflict graph *)
  adjacency : RegSet.t; (* connected register *)
}

module Vertex : sig
  type t

  val compare : t -> t -> int
  val show : t -> string
end

module Graph : sig
  type t

  val empty : t
  val vertex : reg -> t
  val overlay : t -> t -> t
  val connect : t -> t -> t
  val verticies : t -> vertex list
  val show : t -> string
end
