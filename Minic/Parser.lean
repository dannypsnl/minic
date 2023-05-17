import Lean.Data.Parsec
import Lean.Data.Position
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

def tryP (p : Parsec a) : Parsec (Option a) := λ it =>
  match p it with
  | .success rem a => .success rem a
  | .error _ _ => .success it .none

def run' (p : Parsec α) (filepath : System.FilePath) (s : String) : Except String α :=
  match p s.mkIterator with
  | .success _ res => Except.ok res
  | .error it err  =>
    let f := it.s.toFileMap.toPosition it.pos
    Except.error s!"{filepath}:{f}: {err}"

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

  partial def binary (opList : List $ Parsec (MExpr → MExpr → MExpr)) (tm : Parsec MExpr)
    : Parsec MExpr := do
    let l ← tm
    let es ← many opRhs
    return es.toList.foldl (fun lhs e => (e.1 lhs e.2)) l
    where
      opRhs : Parsec $ (MExpr → MExpr → MExpr) × MExpr := do
        let mut op := .none
        for findOp in opList do
          op ← tryP findOp
          if op.isSome then break
        return (op.get!, ← tm)
  partial def mexpr : Parsec MExpr :=
    mterm
    |> binary [keyword "*" *> return .bin .mul, keyword "/" *> return .bin .div]
    |> binary [keyword "+" *> return .bin .add, keyword "-" *> return .bin .sub]
end

def fileParser : Parsec MProg := do
  let e ← mexpr; eof
  return { expr := e }
