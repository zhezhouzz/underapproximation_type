import sys
import argparse
import os
import json

meta_config_file = "meta-config.json"

# names = ["RedBlackTree"]
# names = ["SizedList"]

names = [
    "SizedList",
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
