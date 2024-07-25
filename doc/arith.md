# Arithmetic compilation

You will have your extremely first experience about compilation, the language we supported here is:

```ml
x        (* variable *)
i        (* integer *)
e ::=
  | x
  | i
  | e1 + e2
  | e1 - e2
  | let x := e1 in e2
```

There are 8 passes.

1. **uniquify**
2. **remove complex operands**
3. **explicate control**
4. **select instruction**
5. **liveness analysis**
6. **conflict graph inference**
7. **register allocation**
8. Due to AArch64 stack operations, we add **patch stack operations**

### Run example

To run example, use the following command:

```shell
dune exec minic -- ./example/hello.mml
```

There have three examples in this stage.

1. `hello.mml`
2. `rco_example.mml`
3. `conflict_graph_sample.mml`

### Bonus: move biasing

Remove the instruction like `mov a, a`, this is completely useless but can occur after register allocation.
