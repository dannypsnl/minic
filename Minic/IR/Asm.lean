import Lean.Data.HashMap
import Minic.Ast

namespace Minic.IR.Asm
open Lean
open Lean.HashMap
open Minic.Ast

inductive Stmt
  | assign (name : String) (exp : MExpr)
deriving Repr, BEq

instance : ToString Stmt where
  toString
  | .assign x e => s!"  {x} ← {toString e}"

inductive Tail
  | ret (exp : MExpr)
  | seq (stmt : Stmt) (next : Tail)
deriving Repr, BEq

private def tailToString : Tail → String
  | .ret e => s!"  ret {toString e}"
  | .seq s n => s!"{toString s}\n{tailToString n}"
instance : ToString Tail where
  toString := tailToString

inductive Arch
  | arm64
  | x64
deriving Repr, BEq

-- β stands for lovely block, it's important to provide proper definition for block
structure AsmProg (β : Type) where
  arch : Arch
  blocks : HashMap String β

instance [ToString β] : ToString (AsmProg β) where
  toString p := p.blocks.toList.foldl
    (fun result (name, block) => result ++ s!"{name}:\n{toString block}")
    ""

end Minic.IR.Asm
