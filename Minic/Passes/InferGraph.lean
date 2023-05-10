import Minic.Passes.LivenessAnalysis

namespace Minic.Passes
open Minic.IR.Arm64

-- TODO: which graph?
structure Instr3Block (instr : Type) extends (Instr2Block instr) where
  -- interferenceGraph : Graph Reg
instance [ToString instr] : ToString (Instr3Block instr) where
  toString b := sorry

end Minic.Passes

namespace Minic.Passes.LivenessAnalysis
open Lean
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def pass (p : AsmProg (InstrBlock Arm64Instr))
  : Id (AsmProg (Instr2Block Arm64Instr)) := sorry

end Minic.Passes.LivenessAnalysis
