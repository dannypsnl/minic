import Lean.Data.Parsec
import Lean.Data.Position
import ParsecExtra
import Minic.Ast
open IO
open System
open Lean
open Lean.Parsec
open Parsec.ParseResult
open Minic.Ast

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

  partial def mexpr : Parsec MExpr :=
    mterm
    |> binary [keyword "*" *> return .bin .mul, keyword "/" *> return .bin .div]
    |> binary [keyword "+" *> return .bin .add, keyword "-" *> return .bin .sub]
end

def fileParser : Parsec MProg := do
  let e ← mexpr; eof
  return { expr := e }
