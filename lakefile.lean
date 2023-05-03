import Lake
open Lake DSL

package «minic» {
  -- add package configuration options here
}

lean_lib «Minic» {
  -- add library configuration options here
}

@[default_target]
lean_exe «minic» {
  root := `Main
}
