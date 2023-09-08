open Minic.Compiler

let test_example_if ~filename ~expected_output () =
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
          @@ test_example_if ~filename:"../example/compare.ss"
               ~expected_output:"1";
          test_case "x = 3 result" `Quick
          @@ test_example_if ~filename:"../example/compare2.ss"
               ~expected_output:"0";
        ] );
      ( "boolean",
        [
          test_case "if form" `Quick
          @@ test_example_if ~filename:"../example/boolean_if.ss"
               ~expected_output:"10";
          test_case "if form 2" `Quick
          @@ test_example_if ~filename:"../example/boolean_if2.ss"
               ~expected_output:"15";
          test_case "if form 3" `Quick
          @@ test_example_if ~filename:"../example/boolean_if3.ss"
               ~expected_output:"33";
          test_case "if form 4" `Quick
          @@ test_example_if ~filename:"../example/boolean_if4.ss"
               ~expected_output:"20";
          test_case "(not (not x))" `Quick
          @@ test_example_if ~filename:"../example/boolean.ss"
               ~expected_output:"1";
          test_case "(and x y)" `Quick
          @@ test_example_if ~filename:"../example/boolean2.ss"
               ~expected_output:"0";
          test_case "cond form" `Quick
          @@ test_example_if ~filename:"../example/cond.ss"
               ~expected_output:"34";
        ] );
      ( "arithmetic",
        [
          test_case "hello example" `Quick
          @@ test_example_if ~filename:"../example/hello.ss"
               ~expected_output:"42";
          test_case "remove complex operands" `Quick
          @@ test_example_if ~filename:"../example/rco_example.ss"
               ~expected_output:"61";
          test_case "conflict graph sample" `Quick
          @@ test_example_if ~filename:"../example/conflict_graph_sample.ss"
               ~expected_output:"84";
          test_case "shrink" `Quick
          @@ test_example_if ~filename:"../example/shrink_example.ss"
               ~expected_output:"16";
        ] );
    ]
