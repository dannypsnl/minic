open Ast
open Eio
open AArch64
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

(* Liveness is a backward dataflow problem. With loops the control-flow graph has
   cycles (A -> B -> A), so we cannot analyze a block by recursing into its
   successors — that diverges. Instead we iterate every block round-robin until no
   block's live-before set changes anymore (a fixpoint). Live sets only ever grow
   and the variable universe is finite, so this terminates. *)
let rec run ~(debug : int) (prog : asm) : (label * RegSet.t list) list =
  let table : (label, RegSet.t list) Hashtbl.t = Hashtbl.create 16 in
  (* seed every block with an empty live-in so successors can be looked up *)
  List.iter (fun (label, _) -> Hashtbl.replace table label [ RegSet.empty ]) prog;
  (* live-before the block = live-in = head of its current live set list *)
  let live_in label = List.hd (Hashtbl.find table label) in
  let changed = ref true in
  while !changed do
    changed := false;
    prog
    |> List.iter (fun (label, { instrs; successor; _ }) ->
           let live_after =
             successor
             |> List.fold_left
                  (fun acc s -> RegSet.union acc (live_in s))
                  RegSet.empty
           in
           let live_sets = analyze_instrs instrs [ live_after ] in
           let old_in = List.hd (Hashtbl.find table label) in
           if not (RegSet.equal (List.hd live_sets) old_in) then changed := true;
           Hashtbl.replace table label live_sets)
  done;

  let block_livesets =
    List.map (fun (label, _) -> (label, Hashtbl.find table label)) prog
  in
  if debug >= 1 then (
    traceln "[pass] liveness analysis";
    print_block_livesets prog block_livesets);
  block_livesets

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
