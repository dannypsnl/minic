import Lean.Data.HashMap
import Lean.Data.HashSet
import Minic.Ast
import Minic.IR.Arm64
import Mathlib.Data.Set.Finite

namespace Lean
open Lean
open Lean.HashSet

instance [BEq α] [Hashable α] : Union (HashSet α) where
  union s₁ s₂ := s₁.merge s₂

instance [BEq α] [Hashable α] : SDiff (HashSet α) where
  sdiff s₁ s₂ := HashSet.ofList <| s₁.toArray.filter (λ x => !s₂.contains x)

instance [BEq α] [Hashable α] : Singleton α (HashSet α) where
  singleton e := empty.insert e

end Lean

namespace Minic.IR.Arm64
open Lean
open Lean.HashMap

abbrev LiveSet := HashSet Reg
instance : ToString LiveSet where
  toString set :=
    let x := set.toList
    s!"{x}"

def Arm64Instr.writeSet : Arm64Instr → LiveSet
  | .mov d .. => {d}
  | .addi d .. => {d}
  | .subi d .. => {d}
  | .smul d .. => {d}
  | .sdiv d .. => {d}
  | .ret => ∅
def Src.liveSet : Src → LiveSet
  | Src.sreg r => {r}
  | _ => ∅
def convert (srcs : List Src) : LiveSet :=
  srcs.foldl (fun set src => set ∪ src.liveSet) ∅
def Arm64Instr.readSet : Arm64Instr → LiveSet
  | .mov _ s => convert [s]
  | .addi _ s1 s2 => convert [s1, s2]
  | .subi _ s1 s2 => convert [s1, s2]
  | .smul _ s1 s2 => convert [s1, s2]
  | .sdiv _ s1 s2 => convert [s1, s2]
  | .ret => ∅

end Minic.IR.Arm64

namespace Minic.IR.Asm
open Lean
open Lean.HashMap
open Lean.HashSet
open Minic.Ast
open Minic.IR.Arm64

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
  blocksLiveSet : HashMap String LiveSet := HashMap.empty

instance [ToString β] : ToString (AsmProg β) where
  toString p :=
    let x := p.blocksLiveSet.toList
    p.blocks.toList.foldl
      (fun result (name, block) => result ++ s!"{name}:\n{toString block}")
      ""
    ++ s!"\n\nblocks live sets:\n{x}"

end Minic.IR.Asm
