open Base
open Parsexp_io
open Minic.Ast

let () =
  let filename = (Sys.get_argv ()).(1) in
  let e = load_conv_exn Single ~filename (fun e -> e) in
  (* Stdio.print_endline "stage 0: s-expression";
     Stdio.print_endline (Sexp.to_string e); *)
  let e = e |> expr_from_sexp in
  (* Stdio.print_endline "\nstage 1: to expr";
     Stdio.print_endline ([%derive.show: expr] e); *)
  let e =
    e |> Minic.Pass_uniquify.run |> Minic.Pass_remove_complex_operands.run
  in
  (* Stdio.print_endline "\nstage 2: remove complex operands";
     Stdio.print_endline (show_expr e); *)
  let e = e |> Minic.Pass_explicate_control.run in
  (* Stdio.print_endline "\nstage 3: explicate control";
     Stdio.print_endline (show_ctail e); *)
  let prog = e |> Minic.Pass_select_instruction.run in
  (* Stdio.print_endline "\nstage 4: select instructions";
     Stdio.print_endline (show_asm prog); *)
  let live_sets = prog |> Minic.Pass_liveness_analysis.run in
  let conflict_graph = Minic.Pass_graph_inference.run prog live_sets in
  let prog = Minic.Pass_register_allocation.run prog conflict_graph in
  Stdio.print_endline "\nstage 7: register allocation";
  Stdio.print_endline (show_asm prog);

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
    Stdlib.Sys.command
      "/opt/homebrew/opt/llvm/bin/clang -c _build/output.s -o _build/output.o"
  in
  if status <> 0 then Stdlib.exit 1;
  let status =
    Stdlib.Sys.command
      "/opt/homebrew/opt/llvm/bin/clang _build/main.c _build/output.o"
  in
  if status <> 0 then Stdlib.exit 1;
  let status = Stdlib.Sys.command "./a.out" in
  if status <> 0 then Stdlib.exit 1;
  let status = Stdlib.Sys.command "rm ./a.out" in
  if status <> 0 then Stdlib.exit 1
