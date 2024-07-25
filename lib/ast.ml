module CharSet = Set.Make (Char)

exception Unhandled_sexp of Base.Sexp.t
exception InvalidCharInVariable of char

type atom = [ `Int of int | `Bool of bool | `Var of string ]
[@@deriving show, eq]

type bin_op = EQ | LT | LE | Add | Sub | And | Or [@@deriving show, eq]
type un_op = Not [@@deriving show, eq]

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
  | `Prim of bin_op * surface_expr * surface_expr
  | `UPrim of un_op * surface_expr
  | `Let of string * surface_expr * surface_expr
  | `If of surface_expr * surface_expr * surface_expr
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

and basic_block = { name : label; body : ctail }
and basic_blocks = (label * basic_block) list

let rec validate_varname : string -> string =
 fun x -> String.map validate_char x

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

let show_basic_blocks (blocks : basic_blocks) : string =
  let f = function
    | label, { body = ctail; _ } -> label ^ ":\n" ^ show_ctail ctail
  in
  blocks |> List.map f |> String.concat "\n"
