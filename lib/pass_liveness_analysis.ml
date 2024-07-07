open Ast
open Eio
module LabelSet = Set.Make (String)

let rec print_livesets (prog : block) (live_sets : RegSet.t list) : unit =
  match live_sets with
  | [] -> ()
  | x :: rest ->
      traceln "%s" (show_regset x);
      print_instrs prog rest

and print_instrs ({ name; instrs; successor } : block)
    (live_sets : RegSet.t list) : unit =
  match instrs with
  | [] -> ()
  | x :: rest ->
      traceln "\t%s" (show_instruction x);
      print_livesets { name; instrs = rest; successor } live_sets

let print_block_livesets : asm -> (label * RegSet.t list) list -> unit =
 fun prog block_livesets ->
  block_livesets
  |> List.iter (fun (label, livesets) ->
         traceln "%s:" label;
         print_livesets (List.assoc label prog) livesets)

let rec run ~(debug : int) (prog : asm) : (label * RegSet.t list) list =
  let block_livesets : (label * RegSet.t list) list ref = ref [] in

  (* track that a block is analyzed or not, i.e. we have get its liveset or not *)
  prog
  |> List.iter (fun (_, block) ->
         let _ = analyze_basic_block prog block_livesets block in
         ());

  if debug >= 1 then (
    traceln "[pass] liveness analysis";
    print_block_livesets prog !block_livesets);
  !block_livesets

and compute_live_before (prog : asm)
    (block_livesets : (label * RegSet.t list) list ref) (successor : label list)
    : RegSet.t =
  match successor with
  (* have no dependencies then live before is empty *)
  | [] -> RegSet.empty
  (* have dependencies, so we collect their result *)
  | ss ->
      let succ_blocks = ss |> List.map (fun label -> List.assoc label prog) in
      let live_sets =
        List.map (analyze_basic_block prog block_livesets) succ_blocks
      in
      List.map List.hd live_sets |> List.fold_left RegSet.union RegSet.empty

and analyze_basic_block (prog : asm)
    (block_livesets : (label * RegSet.t list) list ref)
    ({ name = label; instrs; successor } : block) : RegSet.t list =
  let live_before_k_plus_1 =
    compute_live_before prog block_livesets successor
  in
  let live_sets = analyze_instrs instrs [ live_before_k_plus_1 ] in
  block_livesets := (label, live_sets) :: !block_livesets;
  live_sets

and analyze_instrs : instruction list -> RegSet.t list -> RegSet.t list =
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
