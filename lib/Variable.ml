let rec make base_name =
  let tmp = base_name ^ "." ^ Int.to_string !counter in
  counter := !counter + 1;
  tmp

and counter : int ref = ref 1
