import sys
import argparse
import os
import json
import iter_benchs
import run_bench
from tabulate import tabulate

headers = ["", "#Branch" , "Recursive", "#LocalVar" , "#MP" , "#Query" , "(max. #∀,#∃)" , "total (avg. time)(s)"]

def parse_stat ():
    line = None
    with open (resfile) as f:
        line = f.readline().split('&')
        line = [elem.replace("\n", "").replace("$", "").replace(" ", "") for elem in line]
    return line

def show_source(source, name):
    tab = {"elrond": "⬦", "quickchick": "*",  "quickcheck": "◯", "leonidas": "★", "stlc": ""}
    return "{} {}".format(name, tab[source])

def show_is_rec(is_rec, branches):
    if is_rec:
        return [branches, "✓"]
    else:
        return [branches, ""]

def show_is_rec_latex(is_rec, branches):
    if is_rec:
        return "$" + branches + "$&$\\checkmark$"
    else:
        return "$" + branches + "$&"

def show_data(data):
    print("\n")
    lines = []
    for (source, is_rec, res) in data:
        res = [show_source(source, res[0])] + show_is_rec(is_rec, res[1]) + res[2:]
        lines.append(res)
    print(tabulate(lines, headers, tablefmt='orgtbl', numalign="left"))

def latex_name(name):
    name = name.replace("_", "\_")
    name = "\\textsf{" + name + "}"
    # print(name)
    return name

def show_latex_tab(data):
    print("\n")
    print("""
\\toprule
 & \\#Branch & Recursive & \\#LocalVar & \\#MP & \\#Query & (max. \\#$\\forall$,\\#$\\exists$) & total (avg. time)(s)\\\\
\\midrule""")
    for (source, is_rec, res) in data:
        name = latex_name(res[0])
        branch = show_is_rec_latex(is_rec, res[1])
        line = "{} & {} & ${}$ & ${}$ & ${}$ & ${}$ & ${}$ \\\\".format(
            name, branch, res[2], res[3], res[4], res[5], res[6])
        print(line)
    print("\\bottomrule")

if_show_latex = False

if __name__ == '__main__':
    if_verbose = None
    try:
        if sys.argv[1] == "verbose":
            if_verbose = True
    except:
        if_verbose = False
    benchmark_table, resfile = iter_benchs.init ()
    # print(benchmark_table)
    data = []
    for name in iter_benchs.names:
        source, path, is_rec = iter_benchs.get_info_from_name (benchmark_table, name)
        if os.path.exists(resfile):
            os.remove(resfile)
        run_bench.run(path, if_verbose)
        res = parse_stat ()
        # print(res)
        if res[0] == "false":
            print("fail")
            exit(1)
        else:
            res = [name] + res[2:]
            data.append((source, is_rec, res))
    show_data(data)
