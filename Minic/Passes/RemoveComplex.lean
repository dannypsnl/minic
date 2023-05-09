import Minic.Ast
import Lean.Data.HashMap

namespace Minic.Passes.RemoveComplex
open Lean
open Minic.Ast

mutual
  partial def rcoAtom (e : MExpr) : StateM Nat (MExpr × HashMap String MExpr) := do
    match e with
    | .symbol .. | .fixnum .. => pure (e, HashMap.empty)
    | e => do
      let count ← get
      let v := s!"tmp.{count}"
      set (count + 1)
      let e' ← rcoExp e
      return (.symbol v, HashMap.empty.insert v e')

  partial def rcoExp (e : MExpr) : StateM Nat MExpr := do
    match e with
    | .«let» x e body => return .«let» x (← rcoExp e) (← rcoExp body)
    | .bin op a b =>
      let ⟨a, aBinds⟩ ← rcoAtom a
      let ⟨b, bBinds⟩ ← rcoAtom b
      let bindIntro body x v := .«let» x v body
      return bBinds.fold bindIntro <| aBinds.fold bindIntro (.bin op a b)
    | e => pure e
end

def pass (p : MProg) : Id MProg := do
  return { p with expr := (← (rcoExp p.expr).run 1).1 }

end Minic.Passes.RemoveComplex
