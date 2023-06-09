import Std.Data.HashMap.Basic
import Minic.Ast
import Minic.IR.Asm

namespace Minic.Passes
open Minic.IR.Asm

-- A tail with a list of variables in the block
structure TailBlock where
  varSet : List String
  tail : Tail
instance : ToString TailBlock where
  toString b := s!"@variable set = {b.varSet}\n{toString b.tail}"

end Minic.Passes

namespace Minic.Passes.ExplicateControl
open Std
open Minic.Ast
open Minic.IR.Asm

mutual
  def explicateAssign (e : MExpr) (x : String) (cont : Tail) : TailBlock :=
    match e with
    | .«let» y rhs body =>
      let ⟨vars, e'⟩ := explicateAssign rhs y (.seq (.assign x body) cont)
      ⟨x :: vars, e'⟩
    | e => ⟨[x], .seq (.assign x e) cont⟩

  def explicateTail : MExpr → TailBlock
    | .«let» x rhs body =>
      let ⟨vars, body'⟩ := explicateTail body
      let ⟨vars2, e'⟩ := explicateAssign rhs x body'
      ⟨vars ++ vars2, e'⟩
    | e => ⟨[], .ret e⟩
end

def pass (p : MProg) : AsmProg TailBlock :=
  { arch := .arm64, blocks := HashMap.ofList [("_miniexpr", explicateTail p.expr)] }

end Minic.Passes.ExplicateControl
