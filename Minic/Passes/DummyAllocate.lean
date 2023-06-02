import Minic.Passes.InstrSelection

namespace Minic.Passes.DummyAllocate
open Std (HashMap)
open Minic.IR.Asm
open Minic.IR.Arm64

def mkStore (tmpReg : Reg) (d : Dest) : Option Arm64Instr × Dest :=
  match d with
  | .sp => (.some <| .str tmpReg d, tmpReg)
  | d => (.none, d)
def mkLoad (tmpReg : Reg) (s : Src) : Option Arm64Instr × Src :=
  match s with
  | .sreg .sp => (.some <| .ldr tmpReg s, tmpReg)
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
  let (storeD, d) := mkStore .x15 <| d
  let (loadS1, s1) := mkLoad .x15 <| s1
  let (loadS2, s2) := mkLoad .x14 <| s2
  [instr d s1 s2]
  |> patchSource loadS2
  |> patchSource loadS1
  |> patchDest storeD

def handle (hash : HashMap String Reg) (instr : Arm64Instr)
  : List Arm64Instr :=
  match instr with
  | .mov d s =>
    let (storeD, d) := mkStore .x15 <| r d
    let (loadS, s) := mkLoad .x15 <| rs s
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

def regArray : Array Reg :=
  #[-- x0-7 是參數暫存器
    -- x8 儲存副程式傳回位址
    .x9,
    .x10, .x11, .x12, .x13, .x14, .x15, .x16, .x17, .x18,
    -- x19-28 是被呼叫函數的暫存器
    .x19, .x20, .x21, .x22, .x23, .x24, .x25, .x26, .x27, .x28
   ]

def toReg (n : Nat) : Reg :=
  if n < regArray.size then
    regArray.get! n
  else .sp

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
