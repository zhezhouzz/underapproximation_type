import sys
import argparse
import os
import subprocess
import json
import iter_benchs
import run_bench

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

dir = "data/benchmark/elrond/_synth_uniquel/"

meta_config_file = "meta-config.json"

def invoc_cmd(verbose, cmd, output_file):
    if output_file is not None:
        # print("{}:{}".format(output_file, type(output_file)))
        if (verbose):
            print(" ".join(cmd + [">>", output_file]))
        with open(output_file, "a+") as ofile:
            try:
                subprocess.run(cmd, stdout=ofile)
            except subprocess.CalledProcessError as e:
                print(e.output)
    else:
        if (verbose):
            print(" ".join(cmd))
        try:
            subprocess.run(cmd)
        except subprocess.CalledProcessError as e:
            print(e.output)

verbose = True

for i in range(57, 267):
    source = "{}/{}/prog.ml".format(dir, i)
    refine = "{}/_under.ml".format(dir)
    cmd = cmd_prefix + ["coverage-type-check", meta_config_file,
                        source,
                        refine]
    invoc_cmd(verbose, cmd, None)
