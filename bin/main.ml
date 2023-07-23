open Parsexp_io
open Minic.Ast
module P1 = Minic.Pass_uniquify

let () =
  let filename = Sys.argv.(1) in
  let e = load_conv_exn Single ~filename expr_from_sexp in
  let e = P1.pass e in
  print_endline ([%derive.show: expr] e)
