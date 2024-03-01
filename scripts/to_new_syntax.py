# coding=utf8
# the above tag defines encoding for this document and is for Python 2.x compatibility

import re
import os
import sys
import run_bench

regex = r"([a-z]+) : \[\%forall: ([a-z]+)\]"

def subst(filename):
    with open(filename, "r+") as f:
        input =  f.read()
        # print(input)
        result = re.sub(regex, "\\1 : \\2", input, 0, re.MULTILINE)
        # print(result)
        f.seek(0)
        f.write(result)
        f.truncate()
    return

def add_assert(filename):
    with open(filename, "r+") as f:
        input =  f.read()
        result = re.sub(r"let ([a-z_]+) =$", "let[@assert] \\1 =", input, 0, re.MULTILINE)
        f.seek(0)
        f.write(result)
        f.truncate()
    return

path = sys.argv[1]
def do(path):
    if os.path.isfile(path):
        words = path.split('.')
        if (words[-1] == "ml") or (words[-1] == "mli"):
            subst(path)
            if path.split('/')[-1] == "_under.ml" or path.split('/')[-1] == "rty.ml":
                add_assert(path)
            run_bench.show_source_new(path, "raw", False)
    elif os.path.isdir(path):
        for filename in os.listdir(path):
            sub_path = os.path.join(path, filename)
            do(sub_path)
    return

do(path)
