structure Config where
  sourceFile : System.FilePath

def commandOpt : List String → Option Config
  | [] => none
  | [source] => some { sourceFile := source }
  | _ => none
