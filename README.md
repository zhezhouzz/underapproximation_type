# Artifact Guide: Covering All the Bases: Type-based Verification of Test Input Generators

This is the accompanying artifact for the PLDI 2023 submission *Covering All the Bases: Type-based Verification of Test Input Generators* by Zhou et al. This artifact consists of both the OCaml impelementation (**Poirot**) and the Coq formalization of the type system or our core language **λ<sup>TG</sup>** introduced in the paper.

## Getting Started Guide

We recommend machines have at least 8 GB of memory and 8 GB of hard
disk space available when building and running Docker images. All
benchmarks were tested on MacBook Pro 14-inc, 2021, that has an Apple M1 Pro CPU with 16 GB RAM. The estimated execition time in the rest of document also fits this setting.

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

In this section, we provides the instructions to evaluate our artifact. The [first half of this section](#running-benchmarks-of-poirot) describes installation and use of **Poirot**, an OCaml impelementation of the refinement type checker that verifies the coverage property of the test input generators written in OCaml. The [rest of this section](#running-coq-proofs) describes the Coq formalization of the core language **λ<sup>TG</sup>** in the paper and the corresponding soundness theorem.

### Running Benchmarks of Poirot

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

### STLC Benchmark

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
Types to Check:
⊢ sized_list_gen : s:{v:int | (0 <= v)}→[v:int list | (∀ u, ((len v u) => ((0 <= u) ∧ (u <= s))))]

```

+ Print the source code from the given file in given `format`. Before the refinement type check, **Poirot** (line `811` in the paper) would load the Ocaml code (`raw` format), performs the basic type inference (`typed` format), then translate the code in to the Monadic Normal Form (`mnf` format).

```
$ ./main.exe print-source-code <raw | typed | mnf> <config_file> <source_code_file> <refinement_type_file>
```

> For example,

    $ ./main.exe print-source-code typed meta-config.json data/benchmark/quickchick/sizedlist/_prog.ml data/benchmark/quickchick/sizedlist/_under.ml

> will print

```
[(Basic) Typed]:

let rec sized_list_gen = (fun (size : int) ->
   (let ((b : bool)) = ((size : int) == (0 : int) : bool) in
    (if (b : bool)
     then ((nil : int list)  : int list)
     else
       ((let ((b1 : bool)) =
           ((bool_gen : unit -> bool) ((tt : unit)  : unit) : bool) in
         (if (b1 : bool)
          then ((nil : int list)  : int list)
          else
            ((let ((size1 : int)) = ((size : int) - (1 : int) : int) in
              (let ((l : int list)) =
                 ((sized_list_gen : int -> int list) (size1 : int) :
                 int list) in
               (let ((n : int)) =
                  ((int_gen : unit -> int) ((tt : unit)  : unit) : int) in
                ((cons : int -> int list -> int list) (n : int)
                   (l : int list) : int list) : int list) : int list)) :
            int list) : int list)) : int list) : int list) : int list) :
int -> int list)

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

    $ ./main.exe coverage-type-check meta-config.json data/benchmark/quickchick/sizedlist/_prog.ml data/benchmark/quickchick/sizedlist/_under.ml

> The content of `.result` would be:

```
true & sized_list_gen & $4$ & $12$ & $2$ & $11$ & $(7, 9)$ & $0.38(0.03)$
```

You can also turn on the `debug_info.show_typing` in the configuration file (`meta-config.json`) to show the each step of type check. The details about the configuration file is the section [Configuration of Poirot](#configuration-of-poirot).

```
...

Subtyping Check:
size!0:{v:int | (0 <= v)},sized_list_gen:s:{v:int | (0 <= v)}→[v:int list | ((s < size!0) ∧ (s >= 0) ∧ (∀ u, ((len v u) => ((0 <= u) ∧ (u <= s)))))],b:[v:bool | ((v => (size!0 == 0)) ∧ ((size!0 == 0) => v))],
⊢ [v:int list | (∃ b!14, (∃ x!8, (∃ b!15, (∃ x!9, (∃ b1!7, (∃ a!15, (∃ b!16, (∃ x!10, (∃ b!17, (∃ size1!6, (∃ l!5, (∃ x!11, (∃ n!2, (∃ a!16, (∃ x!12, (∃ a!17, (∃ a!18, ((b ∧ (len x!8 0) ∧ (len v 0)) ∨ ((¬ b) ∧ ((b1!7 ∧ (len x!10 0) ∧ (len v 0)) ∨ ((¬ b1!7) ∧ ((size!0 - 1) == size1!6) ∧ (size1!6 < size!0) ∧ (size1!6 >= 0) ∧ (∀ u, ((len l!5 u) => ((0 <= u) ∧ (u <= size1!6)))) ∧ (len l!5 a!17) ∧ (∀ u, (((1 + a!17) == u) => (len x!12 u))) ∧ (len l!5 a!18) ∧ (∀ u, (((1 + a!18) == u) => (len v u))))))))))))))))))))))))] <: [v:int list | (∀ u, ((len v u) => ((0 <= u) ∧ (u <= size!0))))]

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

##### Input File Formats

**Poirot** expects both an input OCaml code listing and an assertion
file. The input code listing is given as a specially formatted
OCaml source file with certain restrictions, and looks as follows:

```c
(* The library signature. *)
module type DT_NAME = sig
  type TP_NAME
  ...
  val VAR: FUNC_TP
  ...
end

(* The type of the client function. *)
val VAR: FUNC_TP

(* The client implementation. *)
let [rec] VAR (VAR: ARG_TP) ... = EXPR
```

### Running Coq Proofs

The Coq proofs of our core language **λ<sup>TG</sup>** are located in the `coq_proof` directory. These proofs may be executed by running `make`, which may take about `10` min.

### Proof Readme of **λ<sup>TG</sup>**
