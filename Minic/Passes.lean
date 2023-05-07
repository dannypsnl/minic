import Minic.Ast
import Minic.Passes.Uniquify
import Minic.Passes.RemoveComplex
import Minic.Passes.ExplicateControl
import Minic.Passes.InstrSelection
import Minic.Passes.LivenessAnalysis

namespace Minic.Passes
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def all (m : MProg) : Except String (AsmProg (Instr2Block Arm64Instr)) := do
  let r ←
    m |> Uniquify.pass
      |> RemoveComplex.pass
      |> ExplicateControl.pass
      |> InstrSelection.pass
  return r |> LivenessAnalysis.pass

end Minic.Passes
