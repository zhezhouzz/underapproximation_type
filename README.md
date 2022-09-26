# underapproximation_type

## Dependency

+ A parser: https://github.com/zhezhouzz/ocaml_parser
+ A util lib: https://github.com/zhezhouzz/zzdatatype
+ A util lib: https://github.com/zhezhouzz/utils.git
+ A type lib: https://github.com/zhezhouzz/normalty.git

You can install them via the following commands:

```
# git clone https://github.com/zhezhouzz/zzdatatype.git
# cd zzdatatype
# opam install .
```

Other dependent libraries will be reported by dune, just follow the instructions it printed. In details, they are:

```
ocaml-base-compiler        4.12.0      Official release 4.12.0
z3                         4.8.14      Z3 solver
merlin                     4.5-412     Editor helper, provides completion, typing and source browsing in Vim and Emacs
dolog                      6.0.0       pinned to version 6.0.0 at git+file:///Users/zhezhou/workspace/research/dolog#master
core_unix                  v0.14.0     Unix-specific portions of Core
core                       v0.14.1     Industrial strength alternative to OCaml's standard library
ocolor                      1.3.0       Print with style in your terminal using Format's semantic tags
```

## Example

```
# dune exec bin/main.exe -- test over-type-check data/customstk/concat.ml data/customstk/_over_1.ml
```
where `data/customstk/concat.ml` is the target program, and `data/customstk/_over_1.ml` is the (overapproximate) refinement type.

The built-in refinement types (e.g., types of data type constructors) will be load during the initialization. This path is setted by the `prim_path:overp` field in the config file `config/config.json`.
