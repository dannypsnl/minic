open Ast
open Graph

module IntInt = struct
  type t = int * int
end

module Int = struct
  type t = int

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let default = 0
end

module G = Imperative.Graph.AbstractLabeled (IntInt) (Int)

exception TODO

let run : asm -> asm * G.t = fun _ -> raise TODO
