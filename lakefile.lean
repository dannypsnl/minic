import Lake
open Lake DSL

package «minic» {
  -- add package configuration options here
}

lean_lib «Minic» {
  -- add library configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"
  @"7e05f55c01e9f79dbbbec7758e1b93716a988536"

@[default_target]
lean_exe «minic» {
  root := `Main
}
