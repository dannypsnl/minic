import Minic.Passes.Uniquify
import Minic.Passes.RemoveComplex
import Minic.Passes.ExplicateControl
import Minic.Passes.InstrSelection
import Minic.Passes.LivenessAnalysis
import Minic.Passes.InferGraph

namespace Minic.Passes
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def share (m : MProg) := do
  m |> Uniquify.pass
    |> RemoveComplex.pass
    |> ExplicateControl.pass
    |> InstrSelection.pass

def all (m : MProg) := do
  let r ← share m
  r |> LivenessAnalysis.pass
    |> InferGraph.pass
    |> (λ x => return x)

end Minic.Passes
