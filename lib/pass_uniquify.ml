open Ast

exception UnboundVariable of string

let rec pass : expr -> expr = fun e -> go [] e

and go : (string * int) list -> expr -> expr =
 fun env e ->
  match e with
  | Int i -> Int i
  | Var x -> (
      match List.assoc_opt x env with
      | Some cnt -> Var (x ^ "." ^ Int.to_string cnt)
      | None -> raise (UnboundVariable x))
  | Prim (op, es) -> Prim (op, Base.List.map es ~f:(go env))
  | Let (x, t, body) ->
      let new_cnt =
        match List.assoc_opt x env with Some cnt -> cnt + 1 | None -> 1
      in
      let env' = (x, new_cnt) :: env in
      Let (x, go env t, go env' body)
