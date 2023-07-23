open Parsexp_io
open Minic.Ast

let () =
  let filename = Sys.argv.(1) in
  let e = load_conv_exn Single ~filename expr_from_sexp in
  let e =
    e |> Minic.Pass_uniquify.run |> Minic.Pass_remove_complex_operands.run
  in
  print_endline ([%derive.show: expr] e)
