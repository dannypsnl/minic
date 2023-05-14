-- import Mathlib.Combinatorics.SimpleGraph.Basic
import Minic.Passes.LivenessAnalysis

namespace Minic.Passes
open Minic.IR.Arm64

structure Instr3Block (instr : Type) extends (Instr2Block instr) where
  -- conflictGraph : SimpleGraph Reg
instance [ToString instr] : ToString (Instr3Block instr) where
  -- TODO: print conflict graph
  toString _ := "built conflict graph: "

end Minic.Passes

namespace Minic.Passes.InferGraph
open Lean
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

def inferGraphBlock (block : Instr2Block Arm64Instr) : Instr3Block Arm64Instr :=
  let edges := block.livenessSets.zipWith block.instructions.toArray compute
    |> Array.toList
    |> List.join
  -- TODO: build graph from edges
  { block with }

def pass (p : AsmProg $ Instr2Block Arm64Instr) : AsmProg $ Instr3Block Arm64Instr := Id.run do
  let mut blocks' : List (String × Instr3Block Arm64Instr) := []
  for (name, block) in p.blocks do
    blocks' := (name, inferGraphBlock block) :: blocks'
  return { p with blocks := blocks' |> HashMap.ofList }

end Minic.Passes.InferGraph
