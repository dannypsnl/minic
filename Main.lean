import «Minic»

abbrev ConfigIO := ReaderT Config IO

def bufsize : USize := 20 * 1024
partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let buf ← stream.read bufsize
  if buf.isEmpty then
    pure ()
  else
    let stdout ← IO.getStdout
    stdout.write buf
    dump stream

def fileStream (filename : System.FilePath) : ExceptT String IO IO.FS.Stream := do
  let fileExists ← filename.pathExists
  if not fileExists then
    throw "no such file"
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (IO.FS.Stream.ofHandle handle)

def start (cfg : Config) : ExceptT String IO Unit := do
  let fs ← fileStream cfg.sourceFile
  let l ← fs.getLine
  IO.println l

def main (args : List String) : IO Unit :=
  match commandOpt args with
  | none => IO.eprintln "Usage"
  | some opt => do
    let result ← (start opt).run
    match result with
    | .ok .. => pure ()
    | .error ε => IO.eprintln s!"failed: {ε}"
