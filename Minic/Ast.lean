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
  | let' (x : String) (e : MExpr) (body : MExpr)
deriving Repr, BEq, Inhabited

structure MProg where
  expr : MExpr
deriving Repr, BEq

end Minic.Ast
