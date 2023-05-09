namespace Minic.Ast

inductive MOp | add | sub | mul | div
deriving Repr, BEq, Inhabited

instance : Coe Char (Option MOp) where
  coe c :=
  match c with
  | '+' => .some .add
  | '-' => .some .sub
  | '*' => .some .mul
  | '/' => .some .div
  | _ => .none

inductive MExpr
  | symbol (x : String)
  | fixnum (v : Int)
  | bin (op : MOp) (a b : MExpr)
  | «let» (x : String) (e : MExpr) (body : MExpr)
deriving Repr, BEq, Inhabited

private def expToString : MExpr → String
  | .«let» x e b => s!"let {x} := {expToString e}; {expToString b}"
  | .fixnum x => toString x
  | .symbol x => x
  | .bin op a b => match op with
    | .add => s!"{expToString a} + {expToString b}"
    | .sub => s!"{expToString a} - {expToString b}"
    | .mul => s!"{expToString a} * {expToString b}"
    | .div => s!"{expToString a} / {expToString b}"
instance : ToString MExpr where
  toString := expToString

structure MProg where
  expr : MExpr
deriving Repr, BEq

end Minic.Ast
