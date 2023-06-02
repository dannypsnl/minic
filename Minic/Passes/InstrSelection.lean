import Minic.Passes.ExplicateControl
import Minic.IR.Arm64

namespace Minic.Passes
open Minic.IR.Arm64

structure InstrBlock extends TailBlock where
  instructions : List Arm64Instr
instance : ToString InstrBlock where
  toString b :=
    b.instructions.foldl
      (fun result instr => s!"{result}\t{toString instr}\n")
      ""

end Minic.Passes

namespace Minic.Passes.InstrSelection
open Std
open Minic.Ast
open Minic.IR.Asm
open Minic.IR.Arm64

def selectAtom (e : MExpr) : Except String Src := do
  match e with
  | .symbol x => return x
  | .fixnum n => return (↑ n)
  | _ => throw "fail"

def selectAssign (dest : Reg) (e : MExpr) : Except String (List Arm64Instr) := do
  match e with
  | .bin .add a b =>
    return [
      .mov dest (← selectAtom a),
      .addi dest dest (← selectAtom b)
    ]
  | .bin .sub a b =>
    return [
      .mov dest (← selectAtom a),
      .subi dest dest (← selectAtom b)
    ]
  | .bin .mul a b =>
    return [
      .mov dest (← selectAtom a),
      .mov (.var "tmp1") (← selectAtom b),
      .smul dest dest (.sreg (.var "tmp1"))
    ]
  | .bin .div a b =>
    return [
      .mov dest (← selectAtom a),
      .mov (.var "tmp") (← selectAtom b),
      .sdiv dest dest (.sreg (.var "tmp"))
    ]
  | e => return  [.mov dest (← selectAtom e)]

def selectStmt (s : Stmt) : Except String (List Arm64Instr) := do
  match s with
  | .assign x s@(.bin .add b (.symbol x')) | .assign x s@(.bin .add (.symbol x') b) =>
    if x == x' then
      return [.addi x (Reg.var x) (← selectAtom b)]
    else selectAssign x s
  | .assign x e => selectAssign x e

def selectTail (t : Tail) : Except String (List Arm64Instr) := do
  match t with
  | .seq s t => return (← selectStmt s) ++ (← selectTail t)
  | .ret e =>
    if atom? e then
      return [.mov (Reg.x0) (← selectAtom e), .ret]
    else
      let r ← selectAssign (Reg.x0) (e)
      return r ++ [.ret]

def selectOnBlock (block : TailBlock) : Except String InstrBlock := do
  let instrs ← selectTail block.tail
  return { block with
    instructions := instrs
    varSet := "tmp" :: "tmp1" :: block.varSet
  }

def pass (p : AsmProg TailBlock) : Except String (AsmProg InstrBlock) :=
  return {
    arch := .arm64,
    blocks := HashMap.ofList
      (← p.blocks.toList.mapM (fun (name, block) => do
        let newBlock ← selectOnBlock block
        return (name, newBlock)))
  }

end Minic.Passes.InstrSelection
