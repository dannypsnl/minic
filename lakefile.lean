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
  @"fe45c329180f4d051f9f19d1a360d9e195532afb"

require graph from git
  "https://github.com/PeterKementzey/graph-library-for-lean4"

@[default_target]
lean_exe «minic» {
  root := `Main
}
