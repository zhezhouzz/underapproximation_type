import sys
import argparse
import os
import json
import iter_benchs
import run_bench

def run(path, if_verbose):
    run_bench.show_source(path, "raw", if_verbose)
    run_bench.show_source(path, "typed", if_verbose)
    run_bench.show_source(path, "mnf", if_verbose)
    run_bench.show_refine(path, if_verbose)
    return

if __name__ == '__main__':
    if_verbose = None
    try:
        if sys.argv[1] == "verbose":
            if_verbose = True
    except:
        if_verbose = False
    benchmark_table, resfile = iter_benchs.init ()
    for name in iter_benchs.names:
        source, path, is_rec = iter_benchs.get_info_from_name (benchmark_table, name)
        run(path, if_verbose)
    for name in iter_benchs.stlc_names:
        source, path, is_rec = iter_benchs.get_info_from_name (benchmark_table, name)
        run(path, if_verbose)
