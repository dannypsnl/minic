import «Minic»
import Minic.IR.Asm
import ParsecExtra
open Minic.IR.Asm
open Minic.Passes

def writeAsm (prog : AsmProg InstrBlock) (handle : IO.FS.Handle)
  : ExceptT String IO Unit :=
  handle.putStr <| toString prog

def writeMain (handle : IO.FS.Handle) : ExceptT String IO Unit := do
  handle.putStrLn "#include <stdio.h>"
  handle.putStrLn "int miniexpr();"
  handle.putStrLn "int main() { printf(\"%d\\n\", miniexpr()); }"

def start (cfg : Config) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile cfg.sourceFile
  match fileParser.runFilename cfg.sourceFile content with
  | .ok r =>
    let r ← Minic.Passes.runnable <| r
    IO.FS.withFile "build/output.s" .write (writeAsm r)
    let _ ← IO.Process.spawn { cmd := "clang", args := #["-c", "build/output.s", "-o", "build/output.o"] }
    IO.FS.withFile "build/main.c" .write writeMain
    let _ ← IO.Process.spawn { cmd := "clang", args := #["-c", "build/main.c", "-o", "build/main.o"] }
    let _ ← IO.Process.spawn { cmd := "clang", args := #["build/output.o", "build/main.o", "-o", "build/a.out"] }
    IO.println "compiled"
  | .error err =>
    IO.eprintln s!"failed {err}"

def main (args : List String) : IO Unit := do
  match commandOpt args with
  | none => IO.eprintln s!"failed to parse arguments"
  | some opt =>
    match (← (start opt).run) with
    | .ok _ => return ()
    | .error ε => IO.eprintln s!"failed to compile: {ε}"
