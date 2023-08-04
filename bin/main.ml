open Base
open Parsexp_io
open Minic.Ast
open Eio

let ( / ) = Eio.Path.( / )

let build_assembly ~cwd prog =
  let asm_file = cwd / "_build/output.s" in
  Eio.Path.save ~create:(`Or_truncate 0o600) asm_file
    (".global _scmresult\n.align 4\n_scmresult:\n" ^ show_asm prog)

let build_c_driver ~cwd =
  let c_file = cwd / "_build/main.c" in
  Eio.Path.save ~create:(`Or_truncate 0o600) c_file
    "#include <stdio.h>\n\
     extern int scmresult();\n\n\
     int main() {\n\
     \tprintf(\"%d\\n\", scmresult());\n\
     \treturn 0;\n\
     }\n"

let () =
  Eio_main.run @@ fun env ->
  (* debug range: 0 ~ 3 *)
  let debug = 3 in
  let filename = (Sys.get_argv ()).(1) in
  let e = load_conv_exn Single ~filename (fun e -> e) in
  if debug >= 3 then
    traceln "[stage0] s-expression\n%s" ([%derive.show: Sexp.t] e);
  let prog =
    e |> expr_from_sexp |> Minic.Pass_uniquify.run
    |> Minic.Pass_shrink.run ~debug
    |> Minic.Pass_remove_complex_operands.run ~debug
    |> Minic.Pass_explicate_control.run ~debug
    |> Minic.Pass_select_instruction.run ~debug
  in
  let live_sets = prog |> Minic.Pass_liveness_analysis.run ~debug in
  let conflict_graph = Minic.Pass_graph_inference.run ~debug prog live_sets in
  let prog =
    Minic.Pass_register_allocation.run ~debug prog conflict_graph
    |> Minic.Pass_move_biasing.run ~debug
    |> Minic.Pass_stack_patch.run ~debug
  in

  let cwd = Eio.Stdenv.cwd env in
  build_assembly ~cwd prog;
  build_c_driver ~cwd;

  let proc_mgr = Eio.Stdenv.process_mgr env in
  Eio.Process.run proc_mgr
    [ "clang"; "-c"; "_build/output.s"; "-o"; "_build/output.o" ];
  Eio.Process.run proc_mgr [ "clang"; "_build/main.c"; "_build/output.o" ];
  let output = Eio.Process.parse_out proc_mgr Eio.Buf_read.line [ "./a.out" ] in
  traceln "result: %s" output;
  Eio.Process.run proc_mgr [ "rm"; "./a.out" ]
