import Graph
import Minic.Passes.LivenessAnalysis

namespace Minic.Passes
open Graph
open Minic.IR.Arm64

structure Instr3Block extends Instr2Block where
  -- `UndirectedGraph` type takes payload type & weight type
  -- we don't need weights, so we use `Unit`
  conflictGraph : UndirectedGraph Reg Unit
instance : ToString Instr3Block where
  toString b := s!"built conflict graph: {b.conflictGraph}"

end Minic.Passes

namespace Minic.Passes.InferGraph
open Std
open Graph.UndirectedGraph
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def compute (liveAfter : LiveSet) : Arm64Instr → List (Reg × Reg)
  | .ret => []
  -- TODO: complete these cases
  | .mov d s => sorry
  | .addi d s1 s2 => sorry
  | .subi d s1 s2 => sorry
  | .smul d s1 s2 => sorry
  | .sdiv d s1 s2 => sorry
  | _ => sorry

def inferGraphBlock (block : Instr2Block) : Instr3Block :=
  let edges := block.livenessSets.zipWith block.instructions.toArray compute
    |> Array.toList
    |> List.join
    |> List.toArray
  let g := .empty
  -- TODO: build graph from edges
  { block with conflictGraph := g }

def pass (p : AsmProg Instr2Block) : AsmProg Instr3Block := Id.run do
  let mut blocks' : List (String × Instr3Block) := []
  for (name, block) in p.blocks.toArray do
    blocks' := (name, inferGraphBlock block) :: blocks'
  return { p with blocks := blocks' |> HashMap.ofList }

end Minic.Passes.InferGraph
