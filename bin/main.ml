open Parsexp_io
open Minic.Ast

let () =
  let filename = Sys.argv.(1) in
  let e = load_conv_exn Single ~filename expr_from_sexp in
  print_endline "stage 1";
  print_endline ([%derive.show: expr] e);
  let e =
    e |> Minic.Pass_uniquify.run |> Minic.Pass_remove_complex_operands.run
    |> Minic.Pass_explicate_control.run
  in
  print_endline "";
  print_endline "stage 3";
  print_endline (show_ctail e);
  let e = e |> Minic.Pass_select_instruction.run in
  print_endline "";
  print_endline "stage 4";
  print_endline (show_asm e)
