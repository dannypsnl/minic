import Lake
open Lake DSL

package «minic» {
  -- add package configuration options here
}

lean_lib «Minic» {
  -- add library configuration options here
}

require std from git
  "https://github.com/leanprover/std4.git"
  @"6932c4ea52914dc6b0488944e367459ddc4d01a6"

require graph from git
  "https://github.com/PeterKementzey/graph-library-for-lean4"

require «parsec-extra» from git "https://github.com/dannypsnl/parsec-extra" @"main"

@[default_target]
lean_exe «minic» {
  root := `Main
}
