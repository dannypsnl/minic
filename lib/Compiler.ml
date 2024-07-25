open AArch64

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

let compile_binary ~proc_mgr ~cwd prog =
  build_assembly ~cwd prog;
  build_c_driver ~cwd;
  Eio.Process.run proc_mgr
    [ "clang"; "-c"; "_build/output.s"; "-o"; "_build/output.o" ];
  Eio.Process.run proc_mgr [ "clang"; "_build/main.c"; "_build/output.o" ];
  let output = Eio.Process.parse_out proc_mgr Eio.Buf_read.line [ "./a.out" ] in
  Eio.Process.run proc_mgr [ "rm"; "./a.out" ];
  output

let compile ~debug ~proc_mgr ~cwd ~filename =
  let e = Parser.parse_file filename in
  let prog =
    e |> Pass_uniquify.run |> Pass_shrink.run ~debug
    |> Pass_remove_complex_operands.run ~debug
    |> Pass_explicate_control.run ~debug
    |> Pass_select_instruction.run ~debug
  in
  let conflict_graph =
    prog
    |> Pass_liveness_analysis.run ~debug
    |> Pass_graph_inference.run ~debug prog
  in
  let prog =
    Pass_register_allocation.run ~debug prog conflict_graph
    |> Pass_move_biasing.run ~debug
    |> Pass_stack_patch.run ~debug
  in
  compile_binary ~proc_mgr ~cwd prog
