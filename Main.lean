import «Minic»
import Minic.IR.Asm
open Minic.IR.Asm

def start (cfg : Config) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile cfg.sourceFile
  match Minic.Passes.all <| ← fileParser.run content with
  | .ok result =>
    IO.println <| toString result
  | .error err =>
    IO.eprintln s!"failed {err}"

def main (args : List String) : IO Unit := do
  match commandOpt args with
  | none => IO.eprintln s!"failed to parse arguments"
  | some opt =>
    match (← (start opt).run) with
    | .ok _ => return ()
    | .error ε => IO.eprintln s!"failed to compile: {ε}"
