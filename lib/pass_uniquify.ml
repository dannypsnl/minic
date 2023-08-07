open Ast

exception UnboundVariable of string

let rec run e = go [] e

and go : (string * int) list -> surface_expr -> surface_expr =
 fun env e ->
  match e with
  | `Int i -> `Int i
  | `Bool b -> `Bool b
  | `Var x -> (
      match List.assoc_opt x env with
      | Some cnt -> `Var (form_var x cnt)
      | None -> raise (UnboundVariable x))
  | `Prim (op, es) -> `Prim (op, Base.List.map es ~f:(go env))
  | `Let (x, t, body) ->
      let new_cnt =
        match List.assoc_opt x env with Some cnt -> cnt + 1 | None -> 1
      in
      let env' = (x, new_cnt) :: env in
      `Let (form_var x new_cnt, go env t, go env' body)
  | `If (c, t, f) -> `If (go env c, go env t, go env f)
  | `Cond cases ->
      `Cond
        (cases
        |> List.map (fun (c, e) ->
               match c with
               (* `else` is keyword in `cond` *)
               | `Var "else" -> (`Var "else", go env e)
               | _ -> (go env c, go env e)))

and form_var : string -> int -> string =
 fun name cnt -> name ^ "." ^ Int.to_string cnt
