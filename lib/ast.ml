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
