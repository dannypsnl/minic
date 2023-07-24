module CharSet = Set.Make (Char)

exception Unhandled_sexp of Base.Sexp.t
exception InvalidCharInVariable of char

type expr =
  | Int of int
  | Var of string
  (* (+ 1 2 3) *)
  | Prim of op * expr list
  (* (let ([x t]) u) *)
  | Let of string * expr * expr
[@@deriving show, eq]

and op = Add | Sub [@@deriving show, eq]

type catom = [ `CInt of int | `CVar of string ] [@@deriving eq]

type ctail = Return of cexpr | Seq of cstmt * ctail [@@deriving eq]
and cstmt = Assign of string * cexpr [@@deriving eq]
and cexpr = [ catom | `CPrim of op * catom * catom ] [@@deriving eq]

type reg = [ `Reg of string | `Sp of int | `Var of string ] [@@deriving eq, ord]

(* aarch64 *)
type instruction =
  (* add x0, x1, x2 *)
  (* 表示 x0 = x1 + x2 *)
  | Add of dest * src * src
  (* sub x0, x1, x2 *)
  (* 表示 x0 = x1 - x2 *)
  | Sub of dest * src * src
  (* mov x0, x1 *)
  (* 表示從 x0 = x1 *)
  | Mov of dest * src
  | Ret
[@@deriving eq]

and asm = instruction list [@@deriving eq]
and dest = reg
and src = [ reg | `Imm of int ] [@@deriving eq]

module Reg = struct
  type t = reg

  let compare a b = [%derive.ord: reg] a b

  let to_src : reg -> src = function
    | `Reg x -> `Reg x
    | `Var x -> `Var x
    | `Sp shift -> `Sp shift
end

module RegSet = Set.Make (Reg)

(* below are helper functions *)
let rec expr_from_sexp : Base.Sexp.t -> expr =
 fun se ->
  match se with
  | List (Atom "+" :: rest) -> Prim (Add, List.map expr_from_sexp rest)
  | List (Atom "-" :: rest) -> Prim (Sub, List.map expr_from_sexp rest)
  | List [ Atom "let"; List [ List [ Atom x; t ] ]; u ] ->
      Let (validate_varname x, expr_from_sexp t, expr_from_sexp u)
  | Atom x -> (
      match Base.Int.of_string_opt x with
      | Some i -> Int i
      | None -> Var (validate_varname x))
  | List _ -> raise (Unhandled_sexp se)

and validate_varname : string -> string = fun x -> String.map validate_char x

and validate_char : char -> char =
 fun c ->
  match CharSet.find_opt c valid_charset with
  | None -> raise (InvalidCharInVariable c)
  | Some _ -> c

and valid_charset : CharSet.t =
  CharSet.add_seq
    (String.to_seq
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-=+-*/<>?!")
    CharSet.empty

let rec show_ctail : ctail -> string = function
  | Return e -> show_cexpr e
  | Seq (s, k) -> show_cstmt s ^ ";\n" ^ show_ctail k

and show_cstmt : cstmt -> string = function
  | Assign (x, e) -> x ^ " = " ^ show_cexpr e

and show_cexpr : cexpr -> string = function
  | `CInt i -> Int.to_string i
  | `CVar x -> x
  | `CPrim (op, a, b) ->
      Format.sprintf "%s %s %s" (show_catom a) (show_op op) (show_catom b)

and show_catom : catom -> string = function
  | `CInt i -> Int.to_string i
  | `CVar x -> x

and show_op : op -> string = function Add -> "+" | Sub -> "-"

let rec show_asm : asm -> string =
 fun prog ->
  List.map (fun i -> "\t" ^ show_instruction i) prog |> String.concat "\n"

and show_instruction : instruction -> string = function
  | Add (d, s1, s2) ->
      Format.sprintf "add %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Sub (d, s1, s2) ->
      Format.sprintf "sub %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Mov (d, s) -> Format.sprintf "mov %s, %s" (show_reg d) (show_src s)
  | Ret -> "ret"

and show_reg : reg -> string = function
  | `Sp shift -> "[sp, " ^ Int.to_string shift ^ "]"
  | `Reg x -> x
  | `Var x -> "@" ^ x

and show_src : src -> string = function
  | `Sp shift -> "[sp, " ^ Int.to_string shift ^ "]"
  | `Reg x -> x
  | `Imm i -> Int.to_string i
  | `Var x -> "@" ^ x

let show_regset : RegSet.t -> string =
 fun set ->
  let f = function
    | `Reg x -> x
    | `Var x -> "@" ^ x
    | `Sp shift -> "[sp, " ^ Int.to_string shift ^ "]"
  in
  "{ " ^ (RegSet.elements set |> List.map f |> String.concat ", ") ^ " }"
