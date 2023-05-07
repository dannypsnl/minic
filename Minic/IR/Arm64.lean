import Minic.Ast

namespace Minic.IR.Arm64
open Minic.Ast

def atom? : MExpr → Bool
  | .symbol .. | .fixnum .. => true
  | _ => false

inductive Reg
  | reg (name : String)
  | var (name : String)
instance : Coe String Reg where
  coe name := .var name
instance : ToString Reg where
  toString
  | .reg x => s!"{x}"
  | .var x => s!"@{x}"
abbrev Dest := Reg

inductive Src
  | sreg : Reg → Src
  | imm : Int → Src
instance : Coe Reg Src where
  coe r := .sreg r
instance : Coe Int Src where
  coe a := .imm a
instance : ToString Src where
  toString
  | .sreg x => toString x
  | .imm v => toString v

inductive Arm64Instr
  | ret
  | mov (dest : Dest) (src : Src)
  | addi (dest : Dest) (src1 src2 : Src)
  | subi (dest : Dest) (src1 src2 : Src)
  | smul (dest : Dest) (src1 src2 : Src)
  | sdiv (dest : Dest) (src1 src2 : Src)

instance : ToString Arm64Instr where
  toString
  | .ret => "ret"
  | .mov dest src => s!"mov {dest} {src}"
  | .addi dest s1 s2 => s!"add {dest} {s1} {s2}"
  | .subi dest s1 s2 => s!"add {dest} {s1} {s2}"
  | .smul dest s1 s2 => s!"add {dest} {s1} {s2}"
  | .sdiv dest s1 s2 => s!"add {dest} {s1} {s2}"

end Minic.IR.Arm64
