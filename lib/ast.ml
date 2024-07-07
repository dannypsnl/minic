module CharSet = Set.Make (Char)

exception Unhandled_sexp of Base.Sexp.t
exception InvalidCharInVariable of char

type atom = [ `Int of int | `Bool of bool | `Var of string ]
[@@deriving show, eq]

type op = EQ | LT | LE | Add | Sub | Not | And | Or [@@deriving show, eq]
type bin_op = EQ | LT | LE | Add | Sub | And | Or [@@deriving show, eq]
type un_op = Not [@@deriving show, eq]

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

type expr =
  [ atom
  | `Binary of bin_op * expr * expr
  | `Unary of un_op * expr
  | `Let of string * expr * expr
  | `If of expr * expr * expr
  | `Set of string * expr
  | `Begin of expr list * expr
  | `While of expr * expr ]
[@@deriving show, eq]

and surface_expr =
  [ atom
  | `Prim of op * surface_expr list
  | `Let of string * surface_expr * surface_expr
  | `If of surface_expr * surface_expr * surface_expr
  | (* (cond {clause}+ *)
    `Cond of (surface_expr * surface_expr) list
  | `Set of string * surface_expr
  | `Begin of surface_expr list
  | `While of surface_expr * surface_expr ]
[@@deriving show, eq]

type catom = [ `CInt of int | `CVar of string ] [@@deriving eq]
type label = string [@@deriving eq]
type cmp_op = [ `Eq | `Ge | `Gt | `Le | `Lt | `And | `Or ] [@@deriving show, eq]

type ctail =
  | Return of cexpr
  | Seq of cstmt * ctail
  | Goto of label
  | If of { cmp : cmp_op; a : catom; b : catom; thn : label; els : label }
[@@deriving eq]

and cstmt = Assign of string * cexpr | AsStmt of ctail [@@deriving eq]

and cexpr =
  [ catom
  | `Not of catom
  | `Add of catom * catom
  | `Sub of catom * catom
  | `And of catom * catom
  | `Or of catom * catom
  | `EQ of catom * catom
  | `LT of catom * catom
  | `LE of catom * catom
  | `Void ]
[@@deriving eq]

and basic_blocks = (label * ctail) list

type reg = [ `Reg of string | `Sp of int | `Var of string ] [@@deriving eq, ord]

(* aarch64 *)
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

and block = label * instruction list
and asm = block list [@@deriving eq]
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

(* below are helper functions *)
let rec expr_from_sexp : Base.Sexp.t -> surface_expr =
 fun se ->
  match se with
  | List [ Atom "while"; cond; body ] ->
      `While (expr_from_sexp cond, expr_from_sexp body)
  | List [ Atom "set!"; Atom name; expr ] ->
      `Set (validate_varname name, expr_from_sexp expr)
  | List (Atom "begin" :: es) -> `Begin (List.map expr_from_sexp es)
  | List (Atom "=" :: rest) -> `Prim (EQ, rest |> List.map expr_from_sexp)
  | List (Atom "<" :: rest) -> `Prim (LT, rest |> List.map expr_from_sexp)
  | List (Atom "<=" :: rest) -> `Prim (LE, rest |> List.map expr_from_sexp)
  | List (Atom "+" :: rest) -> `Prim (Add, rest |> List.map expr_from_sexp)
  | List (Atom "-" :: rest) -> `Prim (Sub, rest |> List.map expr_from_sexp)
  | List [ Atom "not"; t ] -> `Prim (Not, [ expr_from_sexp t ])
  | List (Atom "and" :: rest) -> `Prim (And, rest |> List.map expr_from_sexp)
  | List (Atom "or" :: rest) -> `Prim (Or, rest |> List.map expr_from_sexp)
  | List [ Atom "if"; cond; t; f ] ->
      `If (expr_from_sexp cond, expr_from_sexp t, expr_from_sexp f)
  | List (Atom "cond" :: clauses) -> `Cond (clauses |> List.map clause_from_sexp)
  | List [ Atom "let"; List [ List [ Atom x; t ] ]; u ] ->
      `Let (validate_varname x, expr_from_sexp t, expr_from_sexp u)
  | Atom "#t" -> `Bool true
  | Atom "#f" -> `Bool false
  | Atom x -> (
      match Base.Int.of_string_opt x with
      | Some i -> `Int i
      | None -> `Var (validate_varname x))
  | List _ -> raise (Unhandled_sexp se)

and clause_from_sexp : Base.Sexp.t -> surface_expr * surface_expr =
 fun se ->
  match se with
  | List [ cond; e ] -> (expr_from_sexp cond, expr_from_sexp e)
  | _ -> raise (Unhandled_sexp se)

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
  | Goto l -> "goto " ^ l
  | If { cmp; a; b; thn; els } ->
      Format.sprintf "if %s %s %s\n  then goto %s\n  else goto %s"
        (show_catom a)
        ([%derive.show: cmp_op] cmp)
        (show_catom b) thn els
  | Return e -> show_cexpr e
  | Seq (s, k) -> show_cstmt s ^ ";\n" ^ show_ctail k

and show_cstmt : cstmt -> string = function
  | Assign (x, e) -> x ^ " = " ^ show_cexpr e
  | AsStmt e -> "run: " ^ show_ctail e

and show_cexpr : cexpr -> string = function
  | `Void -> "#void"
  | `CInt i -> Int.to_string i
  | `CVar x -> x
  | `Not a -> Format.sprintf "not %s " (show_catom a)
  | `Add (a, b) -> Format.sprintf "%s + %s" (show_catom a) (show_catom b)
  | `Sub (a, b) -> Format.sprintf "%s - %s" (show_catom a) (show_catom b)
  | `And (a, b) -> Format.sprintf "%s and %s" (show_catom a) (show_catom b)
  | `Or (a, b) -> Format.sprintf "%s or %s" (show_catom a) (show_catom b)
  | `EQ (a, b) -> Format.sprintf "%s =? %s" (show_catom a) (show_catom b)
  | `LT (a, b) -> Format.sprintf "%s < %s" (show_catom a) (show_catom b)
  | `LE (a, b) -> Format.sprintf "%s <= %s" (show_catom a) (show_catom b)

and show_catom : catom -> string = function
  | `CInt i -> Int.to_string i
  | `CVar x -> x

let show_cmp_op : cmp_op -> string = function
  | `And -> "and"
  | `Or -> "or"
  | `Eq -> "eq"
  | `Gt -> "gt"
  | `Ge -> "ge"
  | `Lt -> "lt"
  | `Le -> "le"

let rec show_asm : asm -> string =
 fun prog -> prog |> List.map show_block |> String.concat "\n"

and show_block : block -> string =
 fun (label, instrs) ->
  label ^ ":\n"
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

let show_basic_blocks : basic_blocks -> string =
 fun blocks ->
  let f = function label, ctail -> label ^ ":\n" ^ show_ctail ctail in
  blocks |> List.map f |> String.concat "\n"
