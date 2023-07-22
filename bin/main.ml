open Parsexp_io

let () =  
  let filename = Sys.argv.(1) in 
  let sexp = load_conv_exn Single ~filename: filename (fun a -> a) in
  print_endline (Base.Sexp.to_string sexp)
