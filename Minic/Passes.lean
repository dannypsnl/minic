import Minic.Ast
import Minic.Passes.Uniquify
import Minic.Passes.RemoveComplex

namespace Minic.Passes
open Minic.Ast

def all (m : MProg) : MProg :=
  m |> Uniquify.pass |> RemoveComplex.pass

end Minic.Passes