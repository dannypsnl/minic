import Lean.Data.Parsec
import Minic.Ast
open IO
open System
open Lean
open Lean.Parsec
open Parsec.ParseResult
open Minic.Ast

namespace Lean.Parsec

def getSourcePos : Parsec String.Pos := fun it : String.Iterator =>
  success it it.pos

end Lean.Parsec

def keyword (s : String) := do skipString s; ws
def identifier : Parsec String := many1Chars <| satisfy isValid
  where
    isValid : Char → Bool
    | '-' => true
    | c => c.isAlphanum
def fixnum : Parsec Int := do
  return (← many1Chars <| satisfy (λ c => c.isDigit)).toInt!

mutual
  partial def letBinding : Parsec MExpr := do
    keyword "let"
    let x ← identifier; ws
    keyword ":="
    let e ← mexpr
    keyword ";"
    let b ← mexpr
    return .«let» x e b

  partial def mterm : Parsec MExpr :=
    (letBinding
     <|> .fixnum <$> fixnum
     <|> .symbol <$> identifier)
    <* ws

  partial def mbinary (opList : List Char) (expr : Parsec MExpr) : Parsec MExpr := do
    let l ← expr
    let loop := do
      let op : Option MOp ← satisfy (opList.contains ·)
      ws
      return (op.get!, ← expr)
    let es ← many loop
    return es.toList.foldl (fun lhs e => (.bin e.1 lhs e.2)) l

  partial def mexpr : Parsec MExpr :=
    (mbinary ['+', '-'] (mbinary ['*', '/'] mterm)) <* ws
end

def fileParser : Parsec MProg := do
  let e ← mexpr; eof
  return { expr := e }
