import sys
import argparse
import os
import subprocess
import shutil
import locale

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]
meta_config_file = "meta-config.json"
workdir = ""

def parse_stat (resfile):
    line = None
    with open (resfile) as f:
        line = f.readline().split('&')
        line = [elem.replace("\n", "").replace("$", "").replace(" ", "") for elem in line]
    return line

def invoc_cmd(cmd, output_file, verbose):
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
    resfile = ".result"
    subdirs = []
    for file in os.listdir(dir_str):
        d = os.path.join(dir_str, file)
        if (file == "inputs" or file == "outputs"):
            continue
        else:
            if os.path.isdir(d):
                subdirs.append(d)
    i, successful, failed = 0,0,0
    subdirs.sort(key=lambda x: int(x.split('/')[-1]))
    for diri in subdirs:
        ithprogrfile = "{}/{}".format(diri, "prog.ml")
        if verbose:
            print ("Running Poirot on "+ithprogrfile)
        cmd = cmd_prefix + ["coverage-type-check", meta_config_file,
                            # "{}/{}".format(dir_str, "config.json"),
                            ithprogrfile,
                            "{}/{}".format(dir_str, "_under.ml")]
        invoc_cmd(cmd, None, verbose)
        res = parse_stat (resfile)
        if res[0] == "true":
            successful = successful + 1
        else:
            failed = failed + 1
        i = i+1
    print ("Total "+str(i))
    print ("Rejected "+str(failed))
    print ("Succeeded "+str(successful))
    return str(i), str(failed), str(successful)
