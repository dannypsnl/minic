open Ast

let rec print_liveset : asm -> RegSet.t list -> unit =
 fun prog live_sets ->
  match live_sets with
  | [] -> ()
  | x :: rest ->
      print_string "\t";
      print_endline (show_regset x);
      print_instr prog rest

and print_instr : asm -> RegSet.t list -> unit =
 fun prog live_sets ->
  match prog with
  | [] -> ()
  | x :: rest ->
      print_endline (show_instruction x);
      print_liveset rest live_sets

let rec run : debug:int -> asm -> RegSet.t list =
 fun ~debug prog ->
  let live_sets = List.fold_right analyze_instr prog [ RegSet.empty ] in
  if debug >= 1 then (
    print_endline "\nstage 5: liveness analysis";
    print_liveset prog live_sets);
  live_sets

and analyze_instr : instruction -> RegSet.t list -> RegSet.t list =
 fun cur_instr live_sets ->
  (* L_before(k+1) *)
  let lb = List.hd live_sets in
  (* L_after(k) = L_before(k+1) *)
  let la = lb in
  let lbk = live_before cur_instr la in
  lbk :: live_sets

(* L_before(k) = (L_after(k) - W(k)) U R(k) *)
and live_before : instruction -> RegSet.t -> RegSet.t =
 fun instr live_after ->
  RegSet.diff live_after (written_locations instr)
  |> RegSet.union (read_variables instr)

and read_variables : instruction -> RegSet.t = function
  | Add (_, r1, r2) -> convert [ r1; r2 ]
  | Sub (_, r1, r2) -> convert [ r1; r2 ]
  | Mov (_, r) -> convert [ r ]
  | Ret -> RegSet.singleton (`Reg "x0")

and convert : src list -> RegSet.t = function
  | [] -> RegSet.empty
  | `Reg x :: srcs -> RegSet.union (RegSet.singleton (`Reg x)) (convert srcs)
  | `Var x :: srcs -> RegSet.union (RegSet.singleton (`Var x)) (convert srcs)
  | _ :: srcs -> convert srcs

and written_locations : instruction -> RegSet.t = function
  | Add (d, _, _) -> RegSet.singleton d
  | Sub (d, _, _) -> RegSet.singleton d
  | Mov (d, _) -> RegSet.singleton d
  | Ret -> RegSet.empty
