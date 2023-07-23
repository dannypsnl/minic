exception Unhandled_sexp of Base.Sexp.t
exception ToManyArguments of int

type expr =
  | Int of int
  | Var of string (* (+ 1 2 3) *)
  | Prim of op * expr list (* (let ([x t]) u) *)
  | Let of string * expr * expr
[@@deriving show, eq]

and op = Add | Sub [@@deriving show, eq]

type catom = [ `CInt of int | `CVar of string ]

type ctail = Return of cexpr | Seq of cstmt * ctail
and cstmt = Assign of string * cexpr
and cexpr = [ catom | `CPrim of op * catom list ]

type reg = [ `Reg of string | `Var of string ]

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

and asm = instruction list
and dest = reg
and src = [ reg | `Imm of int ]

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

(* TODO: raise exception for bad name like `123` *)
and validate_varname : string -> string = fun x -> x

let rec show_ctail : ctail -> string = function
  | Return e -> show_cexpr e
  | Seq (s, k) -> show_cstmt s ^ ";\n" ^ show_ctail k

and show_cstmt : cstmt -> string = function
  | Assign (x, e) -> x ^ " = " ^ show_cexpr e

and show_cexpr : cexpr -> string = function
  | `CInt i -> Int.to_string i
  | `CVar x -> x
  | `CPrim (op, [ a; b ]) ->
      Format.sprintf "%s %s %s" (show_catom a) (show_op op) (show_catom b)
  | `CPrim (_, es) -> raise (ToManyArguments (List.length es))

and show_catom : catom -> string = function
  | `CInt i -> Int.to_string i
  | `CVar x -> x

and show_op : op -> string = function Add -> "+" | Sub -> "-"

let rec show_asm : asm -> string =
 fun prog -> List.map show_instruction prog |> String.concat "\n"

and show_instruction : instruction -> string = function
  | Add (d, s1, s2) ->
      Format.sprintf "add %s, %s, %s" (show_dest d) (show_src s1) (show_src s2)
  | Sub (d, s1, s2) ->
      Format.sprintf "sub %s, %s, %s" (show_dest d) (show_src s1) (show_src s2)
  | Mov (d, s) -> Format.sprintf "mov %s, %s" (show_dest d) (show_src s)
  | Ret -> "ret"

and show_dest : dest -> string = function `Reg x -> x | `Var x -> "@" ^ x

and show_src : src -> string = function
  | `Reg x -> x
  | `Imm i -> Int.to_string i
  | `Var x -> "@" ^ x
