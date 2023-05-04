import Lean.Data.Parsec
open IO
open System
open Lean

open Lean.Parsec
open Parsec.ParseResult

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
  | ident (x : String)
  | fixnum (v : Int)
  | bin (op : MOp) (a b : MExpr)
deriving Repr

inductive MDef
  | let' (x : String) (e : MExpr)
deriving Repr

structure MFileAst where
  definitions : List MDef
deriving Repr

def keyword (s : String) := do skipString s; ws
def identifier : Parsec String := many1Chars <| satisfy isValid
  where
    isValid : Char → Bool
    | '-' => true
    | c => c.isAlphanum
def fixnum : Parsec Int := do
  return (← many1Chars <| satisfy (λ c => c.isDigit)).toInt!

def mterm : Parsec MExpr :=
  (.ident <$> identifier
   <|> .fixnum <$> fixnum)
  <* ws

mutual
  partial def mbinary (opList : List Char) (pl : Parsec MExpr) : Parsec MExpr := do
    let l ← pl
    let loop := do
      let op : Option MOp ← satisfy (opList.contains ·)
      ws
      return (op.get!, ← pl)
    let es ← many loop
    return es.toList.foldl (fun lhs e => (.bin e.1 lhs e.2)) l

  partial def mexpr : Parsec MExpr :=
    (mbinary ['+', '-'] (mbinary ['*', '/'] mterm)) <* ws
end

def letBinding : Parsec MDef := do
  guard (← peek?).isSome
  keyword "let"
  let x ← identifier; ws
  keyword ":="
  let e ← mexpr
  keyword ";"
  return .let' x e

def fileParser : Parsec MFileAst := do
  let mut defs := []
  while (← peek?).isSome do
    defs := (← letBinding) :: defs
  eof
  return { definitions := defs.reverse }
