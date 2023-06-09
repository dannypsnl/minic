import Minic.Passes.InstrSelection

namespace Minic.Passes
open Minic.IR.Arm64

structure Instr2Block extends InstrBlock where
  livenessSets : Array LiveSet
instance : ToString Instr2Block where
  toString b :=
    let x := b.livenessSets.toList.map toString
    b.instructions.foldl
      (fun result instr => s!"{result}\t{toString instr}\n")
      ""
    ++ s!"{x}"

end Minic.Passes

namespace Minic.Passes.LivenessAnalysis
open Std
open Std.HashMap
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

def livenessOnBlock (name : String) (block : InstrBlock)
  : StateM (HashMap String LiveSet) Instr2Block := do
  let (_, liveSets) := (livenessAnalysis block.instructions.reverse).run .empty
  let blockToLiveSet ← get
  set (blockToLiveSet.insert name (liveSets.get! 0))
  return { block with
    livenessSets := liveSets
  }

def pass (p : AsmProg InstrBlock) : Id (AsmProg Instr2Block) := do
  let mut blocksLiveSet : HashMap String LiveSet := HashMap.empty
  let mut blocks' : List (String × Instr2Block) := []
  for (name, block) in p.blocks.toArray do
    let (block', curBlkLiveSet) ← (livenessOnBlock name block).run HashMap.empty
    blocks' := (name, block') :: blocks'
    blocksLiveSet := blocksLiveSet.mergeWith (fun _ _ b₂ => b₂) curBlkLiveSet
  return { p with
    blocks := blocks' |> HashMap.ofList,
    blocksLiveSet := blocksLiveSet
  }

end Minic.Passes.LivenessAnalysis
