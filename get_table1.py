import sys
import argparse
import os
import json
import run_bench
from tabulate import tabulate

meta_config_file = "meta-config.json"

headers = ["", "#Branch" , "#LocalVar" , "#MP" , "#Query" , "(max. #∀,#∃)" , "total (avg. time)(s)"]

# names = ["RedBlackTree"]
# names = ["SizedList"]

names = ["SizedList",
         "SortedList",
         "UniqueList",
         "SizedTree",
         "CompleteTree",
         "RedBlackTree",
         "SizedBST",
         "BatchedQueue",
         "BankersQueue",
         "Stream",
         "SizedHeap",
         "LeftistHeap",
         "SizedSet",
         "UnbalanceSet"]

def init ():
    resfile = None
    benchmark_table = None
    with open (meta_config_file) as f:
        j = json.load(f)
        resfile = j['resfile']
        benchmark_table_file = j['benchmark_table_file']
        with open (benchmark_table_file) as f:
            benchmark_table = json.load(f)
    return benchmark_table, resfile

def parse_stat ():
    line = None
    with open (resfile) as f:
        line = f.readline().split('&')
        line = [elem.replace("\n", "").replace("$", "").replace(" ", "") for elem in line]
    return line

def get_info_from_name(tab, name):
    source = None
    path = None
    is_rec = True
    for info in tab['benchmarks_info']:
        for entry in info['benchmarks']:
            if entry["name"] == name:
                source = info["benchmark_source"]
                path = "{}/{}/{}".format(tab['benchmark_dir'], source, entry["path"])
                is_rec = entry["is_rec"]
                break
    return source, path, is_rec

def show_source(source, name):
    tab = {"elrond": "⬦", "quickchick": "*",  "quickcheck": "◯", "leonidas": "★"}
    return "{} {}".format(name, tab[source])

def show_is_rec(is_rec, branches):
    if is_rec:
        return branches + "†"
    else:
        return branches

def show_data(data):
    lines = []
    for (source, is_rec, res) in data:
        res = [show_source(source, res[0]), show_is_rec(is_rec, res[1])] + res[2:]
        lines.append(res)
    print(tabulate(lines, headers, tablefmt='orgtbl', numalign="left"))

if __name__ == '__main__':
    benchmark_table, resfile = init ()
    # print(benchmark_table)
    data = []
    for name in names:
        source, path, is_rec = get_info_from_name (benchmark_table, name)
        run_bench.run(path)
        res = parse_stat ()
        # print(res)
        if res[0] == "false":
            print("fail")
            exit(1)
        else:
            res = [name] + res[2:]
            data.append((source, is_rec, res))
    show_data(data)
