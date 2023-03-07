import sys
import argparse
import os
import os.path
import json
import iter_benchs
import run_bench
import synth_iter_benchs

file_name_mappings = { "UniqueList": "uniquelist",
                       "SizedList": "sizedlist",
                       "SortedList": "sortedlist",
                       "SizedTree": "sizedtree",
                       "SizedBST": "sizedbst"}

def make_cobalt_path(name):
    cobalt_name = file_name_mappings[name]
    if cobalt_name is not None:
        return "../propsynth/output/tests_specsynth/Poirot_benchmarks/Poirot_{}.spec".format(cobalt_name)
    else:
        print("unknown benchmark name: %s".format(name))
        exit(1)


if __name__ == '__main__':
    if_verbose = None
    try:
        if sys.argv[1] == "verbose":
            if_verbose = True
    except:
        if_verbose = False
    benchmark_table, resfile = synth_iter_benchs.init ()
    data = []
    for name in synth_iter_benchs.names:
        # get the spath for the benchmark name from the benchmark_table
        spath = synth_iter_benchs.get_info_from_name (benchmark_table, name)
        cobalt_name = make_cobalt_path(name)
        if os.path.exists(cobalt_name):
            print("The synthesized result of \"{}\" detected, copying...".format(name))
            run_bench.cp_source(cobalt_name, spath, if_verbose)
            run_bench.split_source(spath, if_verbose)
        else:
            print("The synthesized result of \"{}\" not found, break".format(name))
            continue
