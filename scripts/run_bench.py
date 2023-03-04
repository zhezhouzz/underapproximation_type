import sys
import argparse
import os
import subprocess

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

workdir = ""

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

def run(dir_str, verbose):
    cmd = cmd_prefix + ["coverage-type-check", meta_config_file,
                        # "{}/{}".format(dir_str, "config.json"),
                        "{}/{}".format(dir_str, "prog.ml"),
                        "{}/{}".format(dir_str, "_under.ml")]
    invoc_cmd(verbose, cmd, None)

def show_refine(dir_str, verbose):
    cmd = cmd_prefix + ["print-coverage-types", meta_config_file,
                        "{}/{}".format(dir_str, "_under.ml")]
    invoc_cmd(verbose, cmd, None)

if __name__ == '__main__':
    try:
        if sys.argv[2] == "verbose":
            verbose = True
    except:
        verbose = False
    run(sys.argv[1], verbose)
