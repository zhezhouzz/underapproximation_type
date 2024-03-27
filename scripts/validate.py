import sys
import argparse
import os
import subprocess
import shutil
import locale
import run_bench

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]
meta_config_file = "meta-config.json"
workdir = ""
verbose = True

def run_type_check_aux(f):
    cmd = cmd_prefix + ["type-check", meta_config_file, f]
    run_bench.invoc_cmd(verbose, cmd, None)

def run_type_check(dir_str):
    if os.path.isdir(dir_str):
        for f in os.listdir(dir_str):
            run_type_check("{}/{}".format(dir_str, f))
    else:
        _, postfix = os.path.splitext(dir_str)
        if postfix == ".ml":
            run_type_check_aux(dir_str)

if __name__ == '__main__':
    dir_str = sys.argv[1]
    run_type_check(dir_str)
