open Ast

type basic_blocks = (label * ctail) list

val run : debug:int -> rco_expr -> basic_blocks
