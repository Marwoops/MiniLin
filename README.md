# MiniLin

This repository is only here for showcasing purposes and will rarely be updated.

Original and up-ot-date **private** repository can be found on [Gaufre](https://gaufre.informatique.univ-paris-diderot.fr/azizim/stage_l2).

Written under [Adrien Guatto](https://www.irif.fr/~guatto/)'s sharp eye.

## Overview

MiniLin is a minimalistic linear language. It will get an evaluator at some point.

## Building

You need a relatively recent version of OCaml as well as several dependencies.

Assuming you have a working OCaml and OPAM installation, building MiniLin is as
simple as running the the following commands in your terminal.

```shell
$ opam install dune sexplib ppx_deriving ppx_compare alcotest
$ dune build
```
