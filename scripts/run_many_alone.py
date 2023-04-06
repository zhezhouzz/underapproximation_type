import sys
import argparse
import os
import subprocess
import shutil
import locale
import colored

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


if __name__ == '__main__':
    
    dir_str = sys.argv[1] 
    verbose = True  
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
    successp = []
    subdirs.sort(key=lambda x: int(x.split('/')[-1]))
    # programs = ['data/benchmark/leonidas/_synth_sizedbst/135/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/137/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/138/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/142/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/143/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/144/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/147/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/148/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/149/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/150/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/152/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/153/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/154/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/155/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/157/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/158/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/159/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/160/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/162/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/163/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/164/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/165/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/173/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/177/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/178/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/179/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/180/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/182/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/183/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/184/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/185/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/189/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/192/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/193/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/194/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/195/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/197/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/198/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/199/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/200/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/202/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/203/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/204/prog.ml', 'data/benchmark/leonidas/_synth_sizedbst/205/prog.ml']
    # for ithprogrfile in programs:
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
            print ("Success")
            print (ithprogrfile)
            successp.append (ithprogrfile)
        else:
            failed = failed + 1
            print (colored.bold_text ("Failed "+ithprogrfile))
        i = i+1
    print ("Total "+str(i))
    print ("Rejected "+str(failed))
    print ("Succeeded "+str(successful))
    print (successp)

