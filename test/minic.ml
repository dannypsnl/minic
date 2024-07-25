open Minic.Compiler
module Tty = Asai.Tty.Make (Minic.Reporter.Message)

let test_example_if ~filename ~expected_output () =
  let fatal diagnostics =
    Tty.display diagnostics;
    Alcotest.fail "unexpected exit"
  in
  Minic.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  Eio_main.run @@ fun env ->
  let output =
    compile ~debug:0
      ~proc_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env) ~filename
  in
  Alcotest.(check string) "expected output" expected_output output

let () =
  let open Alcotest in
  run "Compiler"
    [
      ( "compare",
        [
          test_case "x <= 3 as result" `Quick
          @@ test_example_if ~filename:"../example/compare.mml"
               ~expected_output:"1";
          test_case "x = 3 result" `Quick
          @@ test_example_if ~filename:"../example/compare2.mml"
               ~expected_output:"0";
        ] );
      ( "boolean",
        [
          test_case "if form" `Quick
          @@ test_example_if ~filename:"../example/boolean_if.mml"
               ~expected_output:"10";
          test_case "if form 2" `Quick
          @@ test_example_if ~filename:"../example/boolean_if2.mml"
               ~expected_output:"15";
          test_case "if form 3" `Quick
          @@ test_example_if ~filename:"../example/boolean_if3.mml"
               ~expected_output:"33";
          test_case "if form 4" `Quick
          @@ test_example_if ~filename:"../example/boolean_if4.mml"
               ~expected_output:"20";
          test_case "(not (not x))" `Quick
          @@ test_example_if ~filename:"../example/boolean.mml"
               ~expected_output:"1";
          test_case "(and x y)" `Quick
          @@ test_example_if ~filename:"../example/boolean2.mml"
               ~expected_output:"0";
        ] );
      ( "arithmetic",
        [
          test_case "hello example" `Quick
          @@ test_example_if ~filename:"../example/hello.mml"
               ~expected_output:"42";
          test_case "remove complex operands" `Quick
          @@ test_example_if ~filename:"../example/rco_example.mml"
               ~expected_output:"61";
          test_case "conflict graph sample" `Quick
          @@ test_example_if ~filename:"../example/conflict_graph_sample.mml"
               ~expected_output:"84";
        ] );
    ]
