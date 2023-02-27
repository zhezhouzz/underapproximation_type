# Poirot: Underapproximate Style Refinement Type Checker

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
# dune exec -- bin/main.exe test under-type-check meta-config.json data/benchmark/quickchick/sizedlist/prog.ml data/benchmark/quickchick/sizedlist/_under.ml
```
where
- the file `meta-config.json` contain the configurations of Poirot.
- the file `data/benchmark/quickchick/sizedlist/prog.ml` contains the target program to be verified.
- the file `data/benchmark/quickchick/sizedlist/_under.ml` contains the coverage refinement types.

## Benchmarks

```
# python3 get_table1.py
```

when add the `verbose` flag, the script will print commands of each benchmark.

```
# python3 get_table1.py verbose
```

## Lines of Code

```
git ls-files | grep .ml | grep -v data | xargs wc -l
```
