open Ast
open Eio
module LabelSet = Set.Make (String)

let rec print_livesets : instruction list -> RegSet.t list -> unit =
 fun prog live_sets ->
  match live_sets with
  | [] -> ()
  | x :: rest ->
      traceln "%s" (show_regset x);
      print_instrs prog rest

and print_instrs : instruction list -> RegSet.t list -> unit =
 fun prog live_sets ->
  match prog with
  | [] -> ()
  | x :: rest ->
      traceln "\t%s" (show_instruction x);
      print_livesets rest live_sets

let print_block_livesets : asm -> (label * RegSet.t list) list -> unit =
 fun prog block_livesets ->
  block_livesets
  |> List.iter (fun (label, livesets) ->
         traceln "%s:" label;
         print_livesets (List.assoc label prog) livesets)

let rec run : debug:int -> asm -> (label * RegSet.t list) list =
 fun ~debug prog ->
  let block_livesets : (label * RegSet.t list) list ref = ref [] in

  Switch.run (fun sw ->
      prog
      |> List.iter (fun (label, instrs) ->
             Fiber.fork ~sw (fun () ->
                 let waiting : LabelSet.t ref =
                   ref
                     (instrs
                     |> List.fold_left
                          (fun acc instr ->
                            match instr with
                            | B label -> LabelSet.add label acc
                            | CBNZ (_, label) -> LabelSet.add label acc
                            | CBZ (_, label) -> LabelSet.add label acc
                            | _ -> acc)
                          LabelSet.empty)
                 in
                 let others_livesets : RegSet.t list list ref = ref [] in
                 while not (LabelSet.is_empty !waiting) do
                   let lookup = !waiting |> LabelSet.elements |> List.hd in
                   match List.assoc_opt lookup !block_livesets with
                   | Some livesets ->
                       waiting := LabelSet.remove lookup !waiting;
                       others_livesets := livesets :: !others_livesets
                   | None -> Fiber.yield ()
                 done;
                 let live_before_k_plus_1 : RegSet.t =
                   List.map List.hd !others_livesets
                   |> List.fold_left RegSet.union RegSet.empty
                 in
                 let live_sets =
                   analyze_block instrs [ live_before_k_plus_1 ]
                 in
                 block_livesets := (label, live_sets) :: !block_livesets)));

  if debug >= 1 then (
    traceln "[pass] liveness analysis";
    print_block_livesets prog !block_livesets);
  !block_livesets

and analyze_block : instruction list -> RegSet.t list -> RegSet.t list =
 fun instrs live_sets -> List.fold_right analyze_instr instrs live_sets

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
  | Csel (_, r1, r2, _) -> RegSet.of_list [ r1; r2 ]
  | Cmp (r, s) -> RegSet.singleton r |> RegSet.union (convert [ s ])
  | B _ -> RegSet.empty
  | CBZ (r, _) | CBNZ (r, _) -> RegSet.singleton r
  | Add (_, r1, r2)
  | Sub (_, r1, r2)
  | Xor (_, r1, r2)
  | Or (_, r1, r2)
  | And (_, r1, r2) ->
      convert [ r1; r2 ]
  | Mov (_, r) -> convert [ r ]
  | Str (s, _, _) -> RegSet.singleton s
  | Ldr _ -> RegSet.singleton (`Reg "sp")
  | Ret -> RegSet.singleton (`Reg "x0")

and convert : src list -> RegSet.t = function
  | [] -> RegSet.empty
  | `Reg x :: srcs -> RegSet.union (RegSet.singleton (`Reg x)) (convert srcs)
  | `Var x :: srcs -> RegSet.union (RegSet.singleton (`Var x)) (convert srcs)
  | _ :: srcs -> convert srcs

and written_locations : instruction -> RegSet.t = function
  | Cmp _ | B _ | CBZ _ | CBNZ _ -> RegSet.empty
  | Csel (d, _, _, _)
  | Add (d, _, _)
  | Sub (d, _, _)
  | Xor (d, _, _)
  | Or (d, _, _)
  | And (d, _, _)
  | Mov (d, _) ->
      RegSet.singleton d
  | Str _ -> RegSet.singleton (`Reg "sp")
  | Ldr (d, _, _) -> RegSet.singleton d
  | Ret -> RegSet.empty
