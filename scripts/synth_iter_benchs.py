import sys
import argparse
import os
import json

meta_config_file = "meta-config.json"

names = [
    "UniqueList",
    # "SizedList",
    #  "SortedList",
    # "SizedTree",
    # "SizedBST"
]


def get_info_from_name(tab, name):
    spath = None
    for info in tab['benchmarks_info']:
        for entry in info['benchmarks']:
            if entry["name"] == name:
                source = info["benchmark_source"]
                spath = "{}/{}/{}".format(tab['benchmark_dir'], source, entry["spath"])
                break
    return spath

def init ():
    resfile = None
    benchmark_table = None
    with open (meta_config_file) as f:
        j = json.load(f)
        resfile = j['resfile']
        benchmark_table_file = j['benchmark_table_file']
        # print (benchmark_table_file)
        with open (benchmark_table_file) as f:
            benchmark_table = json.load(f)
    return benchmark_table, resfile
