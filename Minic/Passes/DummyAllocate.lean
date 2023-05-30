import Minic.Passes.InstrSelection

namespace Minic.Passes.DummyAllocate
open Std
open Minic.IR.Asm
open Minic.IR.Arm64

def handle (hash : HashMap String Reg) : Arm64Instr → Arm64Instr
  | .mov d s => .mov (r d) (rs s)
  | .addi d s1 s2 => .addi (r d) (rs s1) (rs s2)
  | .subi d s1 s2 => .subi (r d) (rs s1) (rs s2)
  | .smul d s1 s2 => .smul (r d) (rs s1) (rs s2)
  | .sdiv d s1 s2 => .sdiv (r d) (rs s1) (rs s2)
  | i => i
  where
    r : Dest → Dest
    | .var v => hash.findD v .x30
    | r => r
    rs : Src → Src
    | .sreg reg => r reg
    | r => r

def toReg : Nat → Reg
  | 0 => .x0 | 1 => .x1 | 2 => .x2 | 3 => .x3 | 4 => .x4 | 5 => .x5
  | 6 => .x6 | 7 => .x7 | 8 => .x8 | 9 => .x9 | 10 => .x10 | 11 => .x11
  | 12 => .x12 | 13 => .x13 | 14 => .x14 | 15 => .x15 | 16 => .x16 | 17 => .x17
  | 18 => .x18 | 19 => .x19 | 20 => .x20 | 21 => .x21 | 22 => .x22 | 23 => .x23
  | 24 => .x24 | 25 => .x25 | 26 => .x26 | 27 => .x27 | 28 => .x28 | 29 => .x29
  | 30 => .x30
  | _ => .x30

def allocate (b : InstrBlock) : InstrBlock := Id.run do
  let mut i := 0
  let mut hash := HashMap.empty
  for var in b.varSet.eraseDups do
    hash := hash.insert var (toReg i)
    i := i + 1
  return { b with
    instructions := b.instructions.map (handle hash)
  }

def pass (p : AsmProg InstrBlock) : AsmProg InstrBlock := Id.run do
  let mut blocks' : List (String × InstrBlock) := []
  for (name, block) in p.blocks.toArray do
    let block' ← allocate block
    blocks' := (name, block') :: blocks'
  return { p with
    blocks := blocks' |> HashMap.ofList,
  }

end Minic.Passes.DummyAllocate
