import «Minic»

def start (cfg : Config) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile cfg.sourceFile
  let result ← fileParser.run content
  IO.println s!"result: {result}"

def main (args : List String) : IO Unit := do
  match commandOpt args with
  | none => IO.eprintln s!"failed to parse arguments"
  | some opt =>
    let result ← (start opt).run
    if not result.isOk then
      IO.eprintln s!"failed to compile"
