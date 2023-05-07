import Minic.Ast
import Minic.Passes.Uniquify
import Minic.Passes.RemoveComplex
import Minic.Passes.ExplicateControl
import Minic.Passes.InstrSelection

namespace Minic.Passes
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def all (m : MProg) : Except String (AsmProg (InstrBlock Arm64Instr)) :=
  m |> Uniquify.pass
    |> RemoveComplex.pass
    |> ExplicateControl.pass
    |> InstrSelection.pass

end Minic.Passes
