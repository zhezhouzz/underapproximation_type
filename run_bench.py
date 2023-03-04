import sys
import argparse
import os
import subprocess
verbose = True

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

workdir = ""

def invoc_cmd(cmd, output_file):
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

if __name__ == '__main__':
    name = sys.argv[1]
    dir_str =name
    cmd = cmd_prefix + ["test", "under-type-check",
                        "{}/{}".format(dir_str, "config.json"),
                        "{}/{}".format(dir_str, "prog.ml"),
                        "{}/{}".format(dir_str, "_under.ml")]
    invoc_cmd(cmd, None)
