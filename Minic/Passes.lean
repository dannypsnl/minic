import Minic.Ast
import Minic.Passes.Uniquify
import Minic.Passes.RemoveComplex
import Minic.Passes.ExplicateControl

namespace Minic.Passes
open Minic.Ast
open Minic.IR.Asm

def all (m : MProg) : AsmProg TailBlock :=
  m |> Uniquify.pass
    |> RemoveComplex.pass
    |> ExplicateControl.pass

end Minic.Passes
