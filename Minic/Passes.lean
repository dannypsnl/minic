import Minic.Passes.Uniquify
import Minic.Ast

namespace Minic.Passes
open Minic.Ast

def all (m : MProg) : MProg :=
  Uniquify.pass m

end Minic.Passes