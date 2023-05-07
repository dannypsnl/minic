import Lean.Data.HashMap
import Minic.Ast
import Minic.IR.Asm
import Minic.IR.Arm64
import Minic.Passes.InstrSelection

namespace Minic.Passes

structure Instr2Block (instr : Type) extends (InstrBlock instr) where
  livenessSet : List String
instance [ToString instr] : ToString (Instr2Block instr) where
  toString b :=
    b.instructions.foldl
      (fun result instr => s!"{result}\t{toString instr}\n")
      ""

end Minic.Passes

namespace Minic.Passes.LivenessAnalysis
open Lean
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def pass (p : AsmProg (InstrBlock Arm64Instr))
  : AsmProg (Instr2Block Arm64Instr) :=
  { p with blocks :=
      p.blocks.toList.map (fun (name, block) =>
        (name, { block with livenessSet := sorry}))
      |> HashMap.ofList
  }
  

end Minic.Passes.LivenessAnalysis
