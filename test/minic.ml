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
      ( "boolean",
        [
          test_case "if" `Quick
          @@ test_example_if ~filename:"../example/boolean_if.ss"
               ~expected_output:"10";
          test_case "if2" `Quick
          @@ test_example_if ~filename:"../example/boolean_if2.ss"
               ~expected_output:"15";
          test_case "if3" `Quick
          @@ test_example_if ~filename:"../example/boolean_if3.ss"
               ~expected_output:"33";
          test_case "not not" `Quick
          @@ test_example_if ~filename:"../example/boolean.ss"
               ~expected_output:"1";
          test_case "and" `Quick
          @@ test_example_if ~filename:"../example/boolean2.ss"
               ~expected_output:"0";
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
