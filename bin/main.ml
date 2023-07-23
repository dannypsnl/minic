open Base
open Parsexp_io
open Minic.Ast

let () =
  let filename = (Sys.get_argv ()).(1) in
  let e = load_conv_exn Single ~filename (fun e -> e) in
  Stdio.print_endline "stage 0: s-expression";
  Stdio.print_endline (Sexp.to_string e);
  let e = e |> expr_from_sexp in
  Stdio.print_endline "\nstage 1: to expr";
  Stdio.print_endline ([%derive.show: expr] e);
  let e =
    e |> Minic.Pass_uniquify.run |> Minic.Pass_remove_complex_operands.run
  in
  Stdio.print_endline "\nstage 2: remove complex operands";
  Stdio.print_endline (show_expr e);
  let e = e |> Minic.Pass_explicate_control.run in
  Stdio.print_endline "\nstage 3: explicate control";
  Stdio.print_endline (show_ctail e);
  let e = e |> Minic.Pass_select_instruction.run in
  Stdio.print_endline "\nstage 4: select instructions";
  Stdio.print_endline (show_asm e)
