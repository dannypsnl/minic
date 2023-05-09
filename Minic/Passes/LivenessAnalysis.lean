import Lean.Data.HashMap
import Mathlib.Data.Set.Finite
import Minic.Ast
import Minic.IR.Asm
import Minic.IR.Arm64
import Minic.Passes.InstrSelection

namespace Minic.IR.Arm64

-- consider HashSet
abbrev LiveSet := Set Reg

def Arm64Instr.writeSet : Arm64Instr → LiveSet
  | .mov d .. => Set.singleton d
  | .addi d .. => Set.singleton d
  | .subi d .. => Set.singleton d
  | .smul d .. => Set.singleton d
  | .sdiv d .. => Set.singleton d
  | .ret => ∅

def Src.liveSet : Src → LiveSet
  | Src.sreg r => Set.singleton r
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

namespace Minic.Passes
open Minic.IR.Arm64

structure Instr2Block (instr : Type) extends (InstrBlock instr) where
  livenessSet : Array LiveSet
instance [ToString instr] : ToString (Instr2Block instr) where
  toString b :=
    b.instructions.foldl
      (fun result instr => s!"{result}\t{toString instr}\n")
      ""
    -- TODO: print livenessSet if could
    ++ s!"liveness"

end Minic.Passes

namespace Minic.Passes.LivenessAnalysis
open Lean
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def liveBefore (liveAfter_k : LiveSet) (k : Arm64Instr) : LiveSet :=
  (liveAfter_k \ k.writeSet) ∪ k.readSet

def livenessAnalysis (instrs : List Arm64Instr) : StateM (Array LiveSet) Unit := do
  for k in instrs do
    let livesets ← get
    let liveBefore_k := liveBefore (livesets.getD 0 ∅) k
    set <| livesets.push liveBefore_k

def livenessOnBlock (block : InstrBlock Arm64Instr) : Instr2Block Arm64Instr :=
  { block with livenessSet := (livenessAnalysis block.instructions.reverse).run .empty |> (·.2) }

def pass (p : AsmProg (InstrBlock Arm64Instr))
  : AsmProg (Instr2Block Arm64Instr) :=
  { p with blocks :=
      p.blocks.toList.map (fun (name, block) =>
        (name, livenessOnBlock block))
      |> HashMap.ofList
  }

end Minic.Passes.LivenessAnalysis
