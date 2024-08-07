# minic

This is a minimalist compiler course, based on [Essential of Compilation](https://iucompilercourse.github.io/IU-Fall-2022/), compiles a limited scheme variant to aarch64 assembly.

## Installation

Before start developing, you will need to run below code to install dependencies

```shell
opam install --deps-only --with-test .
```

## Learning

To use this project to learn how to make a compiler, read the followings by order

1. [Arithmetic compilation](./doc/arith.md)
2. [Boolean and Control flow](./doc/bool.md)

## Usage

Nows, you can run build, test

```shell
dune build
dune runtest
```

, and of course, check the example by the command below

```shell
dune exec minic -- ./example/hello.mml
```

> **main** branch is not stable, there might have some example cannot work, if you're a learner then goes to [Release](https://github.com/dannypsnl/minic/releases) and pick a branch first.
> Each release is a stable version, written examples should be runnable
