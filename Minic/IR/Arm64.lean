import Minic.Ast

namespace Minic.IR.Arm64
open Minic.Ast

def atom? : MExpr → Bool
  | .symbol .. | .fixnum .. => true
  | _ => false

inductive Reg
  | x0 | x1 | x2 | x3 | x4 | x5
  | x6 | x7 | x8 | x9 | x10 | x11
  | x12 | x13 | x14 | x15 | x16 | x17
  | x18 | x19 | x20 | x21 | x22 | x23
  | x24 | x25 | x26 | x27 | x28 | x29
  | x30
  | var (name : String)
deriving Repr, BEq, Hashable
instance : Coe String Reg where
  coe name := .var name
instance : ToString Reg where
  toString
  | .var x => s!"@{x}"
  | .x0  => "x0"
  | .x1  => "x1"
  | .x2  => "x2"
  | .x3  => "x3"
  | .x4  => "x4"
  | .x5  => "x5"
  | .x6  => "x6"
  | .x7  => "x7"
  | .x8  => "x8"
  | .x9  => "x9"
  | .x10 => "x10"
  | .x11 => "x11"
  | .x12 => "x12"
  | .x13 => "x13"
  | .x14 => "x14"
  | .x15 => "x15"
  | .x16 => "x16"
  | .x17 => "x17"
  | .x18 => "x18"
  | .x19 => "x19"
  | .x20 => "x20"
  | .x21 => "x21"
  | .x22 => "x22"
  | .x23 => "x23"
  | .x24 => "x24"
  | .x25 => "x25"
  | .x26 => "x26"
  | .x27 => "x27"
  | .x28 => "x28"
  | .x29 => "x29"
  | .x30 => "x30"
abbrev Dest := Reg

inductive Src
  | sreg (reg : Reg)
  | imm (val : Int)
instance : Coe Reg Src where
  coe r := .sreg r
instance : Coe Int Src where
  coe a := .imm a
instance : ToString Src where
  toString
  | .sreg x => toString x
  | .imm v => s!"#{toString v}"

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
  | .mov dest src => s!"mov {dest}, {src}"
  | .addi dest s1 s2 => s!"add {dest}, {s1}, {s2}"
  | .subi dest s1 s2 => s!"sub {dest}, {s1}, {s2}"
  | .smul dest s1 s2 => s!"mul {dest}, {s1}, {s2}"
  | .sdiv dest s1 s2 => s!"sdiv {dest}, {s1}, {s2}"

end Minic.IR.Arm64
