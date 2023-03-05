# Artifact Guide: Covering All the Bases: Type-based Verification of Test Input Generators

This is the accompanying artifact for the PLDI 2023 submission *Covering All the Bases: Type-based Verification of Test Input Generators* by Zhou et al. This artifact consists of both the OCaml impelementation (**Poirot**) and the Coq formalization of the type system or our core language **λ<sup>TG</sup>** introduced in the paper.

## Getting Started Guide

We recommend machines have at least 8 GB of memory and 8 GB of hard
disk space available when building and running Docker images. These
benchmarks were tested on MacBook Pro 14-inc, 2021, that has an Apple M1 Pro CPU with 16 GB RAM.

### Requirements

This artifact is built as a Docker image. Before proceeding, ensure
Docker is installed. (On *nix, `sudo docker run hello-world` will test
your installation.) If Docker is not installed, install it via the
[official installation guide](https://docs.docker.com/get-docker/). This guide was tested using Docker version `20.10.23**, but any contemporary Docker version is expected to work.

### Using the Pre-Built Docker Image

You may fetch the pre-built Docker image from Docker Hub:

    $ docker pull poirot23/poirot:pldi-2023

### Building the Docker Image (Optional)

Alternately, to build the Docker image yourself, navigate to the
directory containing the Dockerfile and tell Docker to build:

    $ docker build . --tag poirot23/poirot:pldi-2023

This step may take a long time (over `30` min).

**Resource Requirements:** Although our tool **Poirot** and the Coq formalization doesn't have large memory usage, building the docker image needs more than `32GB` RAM available. This memory usage requirement comes from the then installation of the SMT solver `z3` (https://github.com/Z3Prover/z3).
The memory error can be fixed by increasing the memory limit in Docker; you can find instructions for doing so on Mac here: (https://docs.docker.com/desktop/settings/mac/#resources), for Windows here: (https://docs.docker.com/desktop/settings/windows/#resources), and for Linux here: (https://docs.docker.com/desktop/settings/linux/#resources). The pre-built docker image is built on a Linux machine having Intel i7-8700 CPU @ 3.20GHz with `64GB` of RAM.

### Running the Docker Image

To launch a shell in the Docker image, say:

    $ docker run -it poirot23/poirot:pldi-2023

To compile **Poirot**, say:

    $ dune build && cp _build/default/bin/main.exe main.exe

The compliation result of **Poirot** is an executable `_build/default/bin/main.exe`. For the sake of convenience, we copy it under the current directory. You can run **Poirot** by excuting `main.exe <args>` directly, or excuting it via `dune`, that is `dune exec -- bin/main.exe <args>`.

You can print **Poirot**'s help message to verify the tool is operating
successfully:

    $ ./main.exe --help

You can print the refinement type used in the `SizedList` benchmark:

    $ ./main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml

The expected output is:

```
Types to Check:
⊢ sized_list_gen : s:{v:int | (0 <= v)}→[v:int list | (∀ u, ((len v u) => ((0 <= u) ∧ (u <= s))))]

```

### Coq proofs in the Docker Image

The Coq proofs of our core language **λ<sup>TG</sup>** are located in the `coq_proof` directory. These proofs may be executed by running `make`, which may take about `10` min.

    $ cd coq_proof && make

## Step-by-Step Instructions

In this section, we provides the instructions to evaluate our artifact. The first half of this section describes installation and use of **Poirot**, an OCaml impelementation of the refinement type checker that verifies the coverage property of the test input generators written in OCaml. The rest of the document describes the Coq formalization of the core language **λ<sup>TG</sup>** in the paper and the corresponding soundness theorem.

### Running Poirot Benchmarks

##### Comprehensive Scripts

The following scripts run the benchmark suite displayed in Table 1 of the paper, it will take about `50` second:

    $ python3 scripts/get_table1.py

The following scripts run the benchmark suite displayed in Table 2 of the paper:

    $ python3 ???

The following scripts run the `STLC` benchmark suite that asked by the reviewers, it will take about `200` second. The details about this new benchmarks can be found in ??.

    $ python3 scripts/run_stlc.py

All scripts above will print the coresponding table.

##### Detailed Steps

By add command line argument `verbose`, the all scripts above will show the actual command sent to **Poirot** on each benchmark. For example, by runing:

    $ python3 scripts/get_table1.py verbose

The script will print the following commands:

```
dune exec -- bin/main.exe coverage-type-check meta-config.json data/benchmark/stlc/nonderter_dec/prog.ml data/benchmark/stlc/nonderter_dec/_under.ml
dune exec -- bin/main.exe coverage-type-check meta-config.json data/benchmark/stlc/gen_const/prog.ml data/benchmark/stlc/gen_const/_under.ml
...
```

### Detail Usage of Poirot

##### Commands of Poirot

Using **Poirot**, you can

+ Print the refinement types (we called _coverage refinement types_ in the paper) that encodes the coverage property from the given file:

```
$ ./main.exe print-coverage-types <config_file> <refinement_type_file>
```

> For example,

    $ ./main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml

> will print

```
```

+ Print the source code from the given file in given `format`. Before the refinement type check, **Poirot** (line `811` in the paper) would load the Ocaml code (`raw` format), performs the basic type inference (`typed` format), then translate the code in to the Monadic Normal Form (`mnf` format).

```
$ ./main.exe print-source-code <saw | typed | mnf> <config_file> <source_code_file> <refinement_type_file>
```

> For example,

    $ ./main.exe print-source-code typed meta-config.json data/benchmark/quickchick/sizedlist/_prog.ml data/benchmark/quickchick/sizedlist/_under.ml

> will print

```
```

+ Type check the given source code is type safe with respect to the given refinement type (line `810` in the paper):

```
$ ./main.exe coverage-type-check <config_file> <source_code_file> <refinement_type_file>
```

The result of type check is saved in the file `.result` by default. The first word of `.result` indicates if the code is type safe.

```
< true | false > & <statistics information>
```

> For example,

    $ ./main.exe coverage-type-check typed meta-config.json data/benchmark/quickchick/sizedlist/_prog.ml data/benchmark/quickchick/sizedlist/_under.ml

> The content of `.result` would be:

```
true & sized_list_gen & $4$ & $12$ & $2$ & $11$ & $(7, 9)$ & $0.38(0.03)$
```

You can also turn on the `debug_info.show_typing` in the configuration file (`meta-config.json`) to show the each step of type check. The details about the configuration file is the section [Configuration of Poirot](#configuration-of-poirot).

```
Type Infer:
...

Subtyping Check:
...

Task 1, type check succeeded
```


##### Configuration of Poirot

All commnads of **Poirot** will take a universial configuration file (`meta-config.json`) in JSON format as its first argument. Precisely, the JSON file
outputs results in JSON format to some output directory.
- the `debug_info` field controls the debug information output. Precisely, we have the following options:
  + if the `show_preprocess` field is true, **Poirot** will print the preprocess result (line `811` in the paper). It will print the given source code, type code, and the code in Monadic Normal Form.
  + if the `show_typing` field is set as true, **Poirot** will print the type judgement of each step in the type check.
  + if the `show_queries` field is set as true, **Poirot** will print the queries that need to by checked by the SMT solver.
  + if the `show_stat` field is set as true, **Poirot** will print statistics information.
  + if the `show_others` field is set as true, **Poirot** will print any other information (this is for debugging).
- the `resfile` field indicates the path of the output file of type check.
- the `logfile` field indicates the path of the log file of type check.
- the `benchmark_table_file` field indicates the path of benchmarks.
- the `prim_path` field indicates the predefined coverage types for a number of
OCaml primitives, including constants, various arithmetic operators, and data constructors for a
range of datatypes(line `813` to `815` in the paper).

### Running Coq Proofs

The Coq proofs of our core language **λ<sup>TG</sup>** are located in the `coq_proof` directory. These proofs may be executed by running `make`, which may take about `10` min.

### Proof Readme

## Details Explianation

The first half of this section describes installation and use of **Poirot**, an OCaml impelementation of the refinement type checker that verifies the coverage property of the test input generators written in OCaml. The rest of the document describes the Coq formalization of the core language **λ<sup>TG</sup>** in the paper and the corresponding soundness theorem.

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
