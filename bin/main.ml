open Base
open Parsexp_io
open Minic.Ast

let () =
  (* debug range: 0 ~ 3 *)
  let debug = 3 in
  let filename = (Sys.get_argv ()).(1) in
  let e = load_conv_exn Single ~filename (fun e -> e) in
  if debug >= 3 then (
    Stdio.print_endline "[stage0] s-expression";
    Stdio.print_endline (Sexp.to_string e));
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
  Stdio.print_endline "\nresult:";

  let asm_file = Stdio.Out_channel.create "_build/output.s" in
  Stdio.Out_channel.output_string asm_file
    ".global _scmresult\n.align 4\n_scmresult:\n";
  Stdio.Out_channel.output_string asm_file (show_asm prog);
  Stdio.Out_channel.close asm_file;
  let c_file = Stdio.Out_channel.create "_build/main.c" in
  Stdio.Out_channel.output_string c_file
    "#include <stdio.h>\n\
     extern int scmresult();\n\n\
     int main() {\n\
     \tprintf(\"%d\\n\", scmresult());\n\
     \treturn 0;\n\
     }\n";
  Stdio.Out_channel.close c_file;
  let status =
    Stdlib.Sys.command "clang -c _build/output.s -o _build/output.o"
  in
  if status <> 0 then Stdlib.exit 1;
  let status = Stdlib.Sys.command "clang _build/main.c _build/output.o" in
  if status <> 0 then Stdlib.exit 1;
  let status = Stdlib.Sys.command "./a.out" in
  if status <> 0 then Stdlib.exit 1;
  let status = Stdlib.Sys.command "rm ./a.out" in
  if status <> 0 then Stdlib.exit 1
