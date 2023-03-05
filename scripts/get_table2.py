import sys
import argparse
import os
import json
import synth_iter_benchs as synth_iter_benchs
import run_bench
import shutil
import run_many
from tabulate import tabulate




headers = ["Benchmark", "#Total", "#Incomplete", "#Complete"]


def show_data(data):
    lines = []
    for (res) in data:
        lines.append(res)
    print(tabulate(lines, headers, tablefmt='orgtbl', numalign="left"))

if __name__ == '__main__':
    
    benchmark_table = synth_iter_benchs.init ()
    data = []
    
    for name in synth_iter_benchs.names:
        # get the spath for the benchmark name from the benchmark_table
        spath = synth_iter_benchs.get_info_from_name (benchmark_table, name)
        total, numberrejected, numbersuccess = run_many.run(spath)
        res = [name, total, numberrejected, numbersuccess] #sizedTree #Total #Complete
        data.append(res)
    #print the table by the tabulate method
    show_data(data)