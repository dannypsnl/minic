import Minic.Passes.InstrSelection

namespace Minic.Passes.DummyAllocate
open Std (HashMap)
open Minic.IR.Asm
open Minic.IR.Arm64

def mkStore (tmpReg : Reg) (d : Dest) : Option Arm64Instr × Dest :=
  match d with
  | .sp shift => (.some <| .str tmpReg shift, tmpReg)
  | d => (.none, d)
def mkLoad (tmpReg : Reg) (s : Src) : Option Arm64Instr × Src :=
  match s with
  | .sreg (.sp shift) => (.some <| .ldr tmpReg shift, tmpReg)
  | s => (.none, s)

def patchSource (s : Option Arm64Instr) (r : List Arm64Instr) : List Arm64Instr :=
  match s with
  | .none => r
  | .some i => i :: r
def patchDest (d : Option Arm64Instr) (r : List Arm64Instr) : List Arm64Instr :=
  match d with
  | .none => r
  | .some i => r ++ [i]

def superPatch (instr : Dest → Src → Src → Arm64Instr)
  (d : Dest) (s1 s2 : Src) : List Arm64Instr :=
  let (storeD, d) := mkStore .x30 <| d
  let (loadS1, s1) := mkLoad .x30 <| s1
  let (loadS2, s2) := mkLoad .x29 <| s2
  [instr d s1 s2]
  |> patchSource loadS2
  |> patchSource loadS1
  |> patchDest storeD

def handle (hash : HashMap String Reg) (instr : Arm64Instr)
  : List Arm64Instr :=
  match instr with
  | .mov d s =>
    let (storeD, d) := mkStore .x30 <| r d
    let (loadS, s) := mkLoad .x30 <| rs s
    [.mov d s]
    |> patchSource loadS
    |> patchDest storeD
  | .addi d s1 s2 => superPatch .addi (r d) (rs s1) (rs s2)
  | .subi d s1 s2 => superPatch .subi (r d) (rs s1) (rs s2)
  | .smul d s1 s2 => superPatch .smul (r d) (rs s1) (rs s2)
  | .sdiv d s1 s2 => superPatch .sdiv (r d) (rs s1) (rs s2)
  | i => [i]
  where
    r : Dest → Dest
    | .var v =>
      match hash.find? v with
      | .some reg => reg
      | .none => panic! "impossible"
    | r => r
    rs : Src → Src
    | .sreg reg => r reg
    | r => r

def toReg : Nat → Reg
  | 0 => .x0 | 1 => .x1 | 2 => .x2 | 3 => .x3 | 4 => .x4 | 5 => .x5
  | 6 => .x6 | 7 => .x7 | 8 => .x8 | 9 => .x9 | 10 => .x10 | 11 => .x11
  | 12 => .x12 | 13 => .x13 | 14 => .x14 | 15 => .x15 | 16 => .x16 | 17 => .x17
  | 18 => .x18 | 19 => .x19 | 20 => .x20 | 21 => .x21 | 22 => .x22 | 23 => .x23
  | 24 => .x24 | 25 => .x25 | 26 => .x26 | 27 => .x27 | 28 => .x28
  -- preserve x29, x30 for stack str/ldr
  | n => .sp <| (28 - n) * 8

def allocate (b : InstrBlock) : InstrBlock := Id.run do
  let mut i := 0
  let mut hash := HashMap.empty
  for var in b.varSet.eraseDups do
    hash := hash.insert var (toReg i)
    i := i + 1
  return { b with
    instructions := (b.instructions.map (handle hash)).join
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
