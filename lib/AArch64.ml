open Ast

type reg = [ `Reg of string | `Sp of int | `Var of string ] [@@deriving eq, ord]

type instruction =
  | Cmp of reg * src
  | Csel of reg * reg * reg * cmp_op
  (* b label *)
  (* unconditionally jumps to pc-relative label *)
  | B of label
  (* cbz Xn, label *)
  (* conditionally jumps to label if Xn is equal to zero *)
  | CBZ of reg * label
  (* cbnz Xn, label *)
  (* conditionally jumps to label if Xn is not equal to zero *)
  | CBNZ of reg * label
  (* add x0, x1, x2 *)
  (* 表示 x0 = x1 + x2 *)
  | Add of dest * src * src
  (* sub x0, x1, x2 *)
  (* 表示 x0 = x1 - x2 *)
  | Sub of dest * src * src
  (* eor x0, x1, x2 *)
  (* 表示 x0 = x1 xor x2 *)
  | Xor of dest * src * src
  | Or of dest * src * src
  | And of dest * src * src
  (* mov x0, x1 *)
  (* 表示從 x0 = x1 *)
  | Mov of dest * src
  (* str x0, [sp, 8] *)
  (* 表示把 x0 推進 sp+8 的位置 *)
  | Str of reg * reg * int
  (* ldr x0, [sp, 8] *)
  (* 表示把 sp+8 的位置內容載入 x0 *)
  | Ldr of reg * reg * int
  | Ret
[@@deriving eq]

and block = { name : label; instrs : instruction list; successor : label list }
and asm = (label * block) list [@@deriving eq]
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

let add (d, s1, s2) = Add (d, s1, s2)
and sub (d, s1, s2) = Sub (d, s1, s2)
and xor (d, s1, s2) = Xor (d, s1, s2)
and iand (d, s1, s2) = And (d, s1, s2)
and ior (d, s1, s2) = Or (d, s1, s2)

let rec show_asm : asm -> string =
 fun prog -> prog |> List.map show_block |> String.concat "\n"

and show_block : label * block -> string =
 fun (_, { name; instrs; successor = _ }) ->
  name ^ ":\n"
  ^ (instrs
    |> List.map (fun i -> "\t" ^ show_instruction i)
    |> String.concat "\n")

and show_instruction : instruction -> string = function
  | Cmp (r, s) -> Format.sprintf "cmp %s, %s" (show_reg r) (show_src s)
  | Csel (d, r1, r2, op) ->
      Format.sprintf "csel %s, %s, %s, %s" (show_reg d) (show_reg r1)
        (show_reg r2) (show_cmp_op op)
  | B label -> Format.sprintf "b %s" label
  | CBZ (c, label) -> Format.sprintf "cbz %s, %s" (show_reg c) label
  | CBNZ (c, label) -> Format.sprintf "cbnz %s, %s" (show_reg c) label
  | Add (d, s1, s2) ->
      Format.sprintf "add %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Sub (d, s1, s2) ->
      Format.sprintf "sub %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Xor (d, s1, s2) ->
      Format.sprintf "eor %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Or (d, s1, s2) ->
      Format.sprintf "orr %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | And (d, s1, s2) ->
      Format.sprintf "and %s, %s, %s" (show_reg d) (show_src s1) (show_src s2)
  | Mov (d, s) -> Format.sprintf "mov %s, %s" (show_reg d) (show_src s)
  | Str (d, s, shift) ->
      Format.sprintf "str %s, [%s, %d]" (show_reg d) (show_reg s) shift
  | Ldr (d, s, shift) ->
      Format.sprintf "ldr %s, [%s, %d]" (show_reg d) (show_reg s) shift
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
