import Minic.Ast
import Lean.Data.HashMap

namespace Minic.Passes.Uniquify

open Lean
open Minic.Ast

def convention (name : String) (n : Nat) : String := s!"{name}.{n}"

def uniquifyExpr (env : HashMap String Nat) : (e : MExpr) → MExpr
  | .symbol x => .symbol <| convention x (env.find! x)
  | .let' x e body =>
    let xId := 1 + (env.find? x).getD 0
    let env' := env.insert x xId
    .let' (convention x xId) (uniquifyExpr env e) (uniquifyExpr env' body)
  | .bin op a b => .bin op (uniquifyExpr env a) (uniquifyExpr env b)
  | e => e

def pass (p : MProg) : MProg :=
  { p with expr := uniquifyExpr HashMap.empty p.expr }

end Minic.Passes.Uniquify
