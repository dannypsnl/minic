# Boolean and Control flow

The language we supported here is:

```ml
x        (* variable *)
i        (* integer *)
b        (* boolean *)
e ::=
  | ...
  | b
  | ...
  | if e1 then e2 else e3
```

The major different part is that `asm` is not `instruction list` anymore, but `(label * block) list` where each `block` contains a list of instructions. This is because we have to introduce jump in assembly now, which break program into several pieces we call _basic block_, and we add the following function

```
explicate_pred : rco_expr -> ctail -> ctail -> ctail
```

you must figure out how to insert a new basic block during this process.
