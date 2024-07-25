open Ast

type rco_expr =
  [ atom
  | `Binary of bin_op * atom * atom
  | `Unary of un_op * atom
  | `Let of string * rco_expr * rco_expr
  | `If of rco_expr * rco_expr * rco_expr
  | `Set of string * rco_expr
  | `Begin of rco_expr list * rco_expr
  | `While of rco_expr * rco_expr ]
[@@deriving show, eq]
