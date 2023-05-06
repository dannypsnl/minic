import «Minic»

def start (cfg : Config) : ExceptT String IO Unit := do
  let content ← IO.FS.readFile cfg.sourceFile
  let result := Minic.Passes.all <| ← fileParser.run content
  IO.println s!"result: {result |> reprStr}"

def main (args : List String) : IO Unit := do
  match commandOpt args with
  | none => IO.eprintln s!"failed to parse arguments"
  | some opt =>
    match (← (start opt).run) with
    | .ok _ => return ()
    | .error ε => IO.eprintln s!"failed to compile: {ε}"
