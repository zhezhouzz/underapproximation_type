# Artifact Guide: Covering All the Bases: Type-based Verification of Test Input Generators

This is the accompanying artifact for the PLDI 2023 submission *Covering All the Bases: Type-based Verification of Test Input Generators* by Zhou et al. This artifact consists of both the OCaml impelementation (**Poirot**) and the Coq formalization of the type system or our core language **λ<sup>TG</sup>** introduced in the paper.

## Getting Started Guide

We recommend machines have at least 16 GB of memory and 4 GB of hard
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

**Resource Requirements:** Although our tool **Poirot*** and the Coq formalization doesn't have large memory usage, building the docker image needs more than 16GB RAM available. This memory usage requirement comes from the then installation of the SMT solver `z3` (https://github.com/Z3Prover/z3). The memory error can be fixed by increasing the memory limit in Docker; you can find instructions for doing so on Mac here: (https://docs.docker.com/desktop/settings/mac/#resources), for Windows here: (https://docs.docker.com/desktop/settings/windows/#resources), and for Linux here: (https://docs.docker.com/desktop/settings/linux/#resources).

### Running the Docker Image

To launch a shell in the Docker image, say:

    $ docker run -it poirot23/poirot:pldi-2023

To compile **Poirot**, say:

    $ dune build

The executable file of **Poirot** is the file `main.exe` under the current directory, which is actually a copy of `_build/default/bin/main.exe`. You can use excute `main.exe <args>` directly, or execute it via `dune`, that is `dune exec -- bin/main.exe <args>`.

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

The Coq proofs of our core language **λ<sup>TG</sup>** are located in the `coq_proof` directory.

## Step-by-Step Instructions

In this section, we provides the instructions to evaluate our artifact. The first half of this section describes installation and use of **Poirot**, an OCaml impelementation of the refinement type checker that verifies the coverage property of the test input generators written in OCaml. The rest of the document describes the Coq formalization of the core language **λ<sup>TG</sup>** in the paper and the corresponding soundness theorem.

### Running Poirot Benchmarks

##### Comprehensive Scripts

The following scripts run the benchmark suite displayed in Table 1 of the paper:

    $ python3 scripts/get_table1.py

The following scripts run the benchmark suite displayed in Table 2 of the paper:

    $ python3 ???

The following scripts run the `STLC` benchmark suite that asked by the reviewers. The details about this new benchmarks can be found in ??.

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

### Detial Usage of Poirot

##### Commands of Poirot

Using Poirot, you can
+ print the source code from the given file:

```
$ ./main.exe print-source-code <config_file> <source_code_file>
```

+ print the refinement types (we called _coverage refinement types_ in the paper) that encodes the coverage property from the given file:

```
$ ./main.exe print-coverage-types <config_file> <refinement_type_file>
```

+ type check the given source code is type safe with respect to the given refinement type:

```
$ ./main.exe coverage-type-check <config_file> <source_code_file> <refinement_type_file>
```

### Running Coq Proofs

The Coq proofs of our inferred specifications are located in the
`coq_proof` directory. These proofs may be executed by running `make`.

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
