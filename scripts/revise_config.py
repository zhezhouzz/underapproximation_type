import re
import sys
import json
import run_bench

import iter_benchs

def modify_file(fname):
    with open(fname, "r") as f:
        j = json.load(f)
        new_j = {'method_predicates': j['all_mps'], 'measure': j['measure'], 'underp': j['prim_path']['underp']}
    with open(fname, "w") as f:
        f.write(json.dumps(new_j))
    return

def modify_refine(path, mps):
    txt = "external method_predicates : t = {}\n\n".format(' '.join([ "\"{}\"".format(mp) for mp in mps]))
    fname = "{}/_under.ml".format(path)
    # print("{}/_under.ml".format(path))
    # print(txt)
    with open(fname, "r") as f:
        content = f.read()
    with open(fname, "w") as f:
        f.write(txt)
        f.write(content)
    return

if __name__ == '__main__':
    benchmark_table, resfile = iter_benchs.init ()
    for name in iter_benchs.names:
        source, path, is_rec = iter_benchs.get_info_from_name (benchmark_table, name)
        run_bench.show_refine(path, False)
        # config_path = "{}/{}".format(path, "config.json")
        # with open(config_path, "r") as f:
            # j = json.load(f)
            # print("{}: {} => {} => {}".format(config_path, j['method_predicates'], j['measure'], j["underp"]))
            # modify_refine(path, j['method_predicates'])
        # modify_file(config_path)
