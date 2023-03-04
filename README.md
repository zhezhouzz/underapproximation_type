# Artifact Guide: Covering All the Bases: Type-based Verification of Test Input Generators

This is the accompanying artifact for the PLDI 2023 submission *Covering All the Bases: Type-based Verification of Test Input Generators* by Zhou et al. This artifact consists of both the OCaml impelementation (**Poirot**) and the Coq formalization of the type system introduced in the paper.

## Getting Started Guide

We recommend machines have at least 4 GB of memory and 4 GB of hard
disk space available when building and running Docker images. These
benchmarks were tested on MacBook Pro 14-inc, 2021, that has an Apple M1 Pro CPU with 16 GB RAM.

### Requirements

This artifact is built as a Docker image. Before proceeding, ensure
Docker is installed. (On *nix, `sudo docker run hello-world` will test
your installation.) If Docker is not installed, install it via the
[official installation guide](https://docs.docker.com/get-docker/). This guide was tested using Docker version `20.10.23`, but any contemporary Docker version is expected to work.

### Using the Pre-Built Docker Image

You may fetch the pre-built Docker image from Docker Hub:

    # docker pull elrondinfer/elrond:2021-oopsla

### Building the Docker Image

Alternately, to build the Docker image yourself, navigate to the
directory containing the Dockerfile and tell Docker to build:

    # docker build . --tag elrondinfer/elrond:2021-oopsla

### Running the Docker Image

To launch a shell in the Docker image, say:

    # docker run -it elrondinfer/elrond:2021-oopsla

You can print **Poirot**'s help message to verify the tool is operating
successfully:

    $ ./main.exe --help

You can print the coverage refinement type used in the `SizedList` benchmark:

    $ ./main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml

The expected output is:

```
Types to Check:
⊢ sized_list_gen : s:{v:int | (0 <= v)}→[v:int list | (∀ u, ((len v u) => ((0 <= u) ∧ (u <= s))))]

```

When you are finished using the image, you may stop it by terminating
the shell with `exit`.

## Step-by-Step Instructions

The first half of this section describes installation and use of **Poirot**, an OCaml impelementation of the refinement type checker that verifies the coverage property of the test input generators written in OCaml. The rest of the document describes the Coq formalization of the core language **λ<sup>TG</sup>** in the paper and the corresponding soundness theorem.

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
dune                        3.1.0       Fast, portable, and opinionated build system
core                       v0.15.0     Industrial strength alternative to OCaml's standard library
core_unix                  v0.15.0     Unix-specific portions of Core
yojson                      1.7.0       Yojson is an optimized parsing and printing library for the JSON format
z3                         4.8.14      Z3 solver
qcheck                      0.18.1      Compatibility package for qcheck
merlin                     4.5-412     Editor helper, provides completion, typing and source browsing in Vim and Emacs
ocolor                      1.3.0       Print with style in your terminal using Format's semantic tags
```

## Example

- Print the refinement types in the given file.

```
# dune exec -- bin/main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml
```

- Type check a program agaisnt the given type.
  + The file `meta-config.json` contain the configurations of Poirot.
  + The file `data/benchmark/quickchick/sizedlist/prog.ml` contains the target program to be verified.
  + The file `data/benchmark/quickchick/sizedlist/_under.ml` contains the coverage refinement types.
  + By default, the verification result and statistics will be saved in the file `.result`.
  + Set the field `debug_info.show_typing` in `meta-config.json` as `true` to show the typing details.

```
# dune exec -- bin/main.exe under-type-check meta-config.json data/benchmark/quickchick/sizedlist/prog.ml data/benchmark/quickchick/sizedlist/_under.ml
```

## Benchmarks

```
# python3 scripts/get_table1.py
```

when add the `verbose` flag, the script will print commands of each benchmark.

```
# python3 scripts/get_table1.py verbose
```

## Lines of Code

```
git ls-files | grep .ml | grep -v data | xargs wc -l
```
