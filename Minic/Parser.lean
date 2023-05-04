import Lean.Data.Parsec
open Lean
open Lean.Parsec

inductive Ast
  | ident (x : String)
  | add

instance : ToString Ast where
  toString a := match a with
  | .ident s => s
  | .add => "add"

def identifier : Parsec String := many1Chars <| satisfy isValid
  where
    isValid : Char → Bool
    | '-' => true
    | c => c.isAlphanum

--   x <- takeWhile1P Nothing isValidChar
--   guard (not (isKeyword x))
--   x <$ whitespace
--   where
--     isValidChar :: Char -> Bool
--     isValidChar '-' = True
--     isValidChar c = isAlphaNum c

def fileParser : Parsec Ast := do
  let s ← identifier
  return .ident s
