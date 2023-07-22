open Base
open List

exception Unhandled_sexp of Sexp.t

type expr =
  | Int of int
  | Var of string (* (+ 1 2 3) *)
  | Prim of op * expr list (* (let ([x t]) u) *)
  | Let of string * expr * expr
[@@deriving show, eq]

and op = Add | Sub [@@deriving show, eq]

let rec expr_from_sexp : Sexp.t -> expr =
 fun se ->
  match se with
  | List (Atom "+" :: rest) -> Prim (Add, map rest ~f:expr_from_sexp)
  | List (Atom "-" :: rest) -> Prim (Sub, map rest ~f:expr_from_sexp)
  | List [ Atom "let"; List [ Atom x; t ]; u ] ->
      Let (validate_varname x, expr_from_sexp t, expr_from_sexp u)
  | Atom x -> (
      match Int.of_string_opt x with
      | Some i -> Int i
      | None -> Var (validate_varname x))
  | List _ -> raise (Unhandled_sexp se)

(* TODO: raise exception for bad name like `123` *)
and validate_varname : string -> string = fun x -> x
