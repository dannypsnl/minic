module Tty = Asai.Tty.Make (Minic.Reporter.Message)

let () =
  let open Eio in
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Minic.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  Eio_main.run @@ fun env ->
  (* debug range: 0 ~ 3 *)
  let debug = 3 in
  let filename = (Base.Sys.get_argv ()).(1) in
  let output =
    Minic.Compiler.compile ~debug
      ~proc_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env) ~filename
  in
  traceln "result: %s" output
