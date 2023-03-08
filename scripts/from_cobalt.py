import sys
import argparse
import os
import os.path
import json
import iter_benchs
import run_bench
import synth_iter_benchs
import colored

file_name_mappings = { "UniqueList": ("uniquelist", ("-cdcl -bi -k 4 -nested 2".split(' '))),
                       "SizedList": ("sizedlist", ("-cdcl -bi -k 3 -nested 3".split(' '))),
                       "SortedList": ("sortedlist", "-cdcl -bi -k 3 -nested 2".split(' ')),
                       "SizedTree": ("sizedtree", "-cdcl -bi -k 5 -nested 4".split(' ')),
                       "SizedBST": ("sizedbst", "-cdcl -bi -k 3 -nested 3".split(' '))
                      }

cobalt_dir = "../propsynth"

def make_cobalt_cmd(name):
    cobalt_name, cmd = file_name_mappings[name]
    if cobalt_name is not None:
        input = "tests_specsynth/Poirot_benchmarks/Poirot_{}.spec".format(cobalt_name)
        output = "output/Poirot_{}.spec".format(cobalt_name)
        return (input, cmd, output)
    else:
        print("unknown benchmark name: {}".format(name))
        exit(1)

def do(name, if_verbose):
    cobalt_output_dir = "{}/output".format(cobalt_dir)
    if not os.path.isdir(cobalt_output_dir):
        os.makedirs(cobalt_output_dir)
    benchmark_table, resfile = synth_iter_benchs.init ()
    if name in synth_iter_benchs.names:
        spath = synth_iter_benchs.get_info_from_name (benchmark_table, name)
        input, cmd, output = make_cobalt_cmd(name)
        print(colored.bold_text("Synthesizing \"{}\" in the directory \"{}\"...".format(name, cobalt_dir)))
        run_bench.run_cobalt(cobalt_dir, cmd, input, if_verbose)
        output_file = "{}/{}".format(cobalt_dir, output)
        if os.path.exists(output_file):
            print(colored.bold_text("The synthesized result of \"{}\" detected, copying...".format(name)))
            run_bench.reset_source(spath, if_verbose)
            run_bench.cp_source(output_file, spath, if_verbose)
            run_bench.split_source(spath, if_verbose)
        else:
            print(colored.bold_text("The synthesized result of \"{}\" not found".format(name)))
    else:
        print(colored.bold_text("unknown benchmark name: {}".format(name)))
    return


if __name__ == '__main__':
    if_verbose = None
    try:
        if sys.argv[2] == "verbose":
            if_verbose = True
    except:
        if_verbose = False
    name = sys.argv[1]
    if name == "all":
        for name in synth_iter_benchs.names:
            do(name, if_verbose)
    else:
        do(name, if_verbose)
