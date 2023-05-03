# MiniLin

## Overview

MiniLin is a minimalistic linear language. It will get a type checker and
evaluator at some point.

## Building

You need a relatively recent version of OCaml as well as several dependencies.

Assuming you have a working OCaml and OPAM installation, building MiniLin is as
simple as running the the following commands in your terminal.

```shell
$ opam install dune sexplib ppx_deriving ppx_compare alcotest
$ dune build
```
