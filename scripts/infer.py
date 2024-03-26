import sys
import argparse
import os
import subprocess
import shutil
import locale
import run_bench
import re

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]
config_file = "meta-config.json"
workdir = ""
verbose = True

def run_type_infer_aux(meta_config_file, f):
    cmd = cmd_prefix + ["type-infer", meta_config_file, f]
    run_bench.invoc_cmd(verbose, cmd, None)

def run_type_infer(dir_str):
    meta_config_file = "{}/{}".format(dir_str, config_file)
    for filename in os.listdir(dir_str):
        matches = re.search(r"prog[0-9]+\.ml$", filename, re.MULTILINE)
        if matches:
            run_type_infer_aux(meta_config_file, "{}/{}".format(dir_str, filename))

if __name__ == '__main__':
    dir_str = sys.argv[1]
    run_type_infer(dir_str)
