import re
import sys
import json

import iter_benchs

def modify_file(fname):
    with open(fname, "r") as f:
        j = json.load(f)
        new_j = {'method_predicates': j['all_mps'], 'measure': j['measure'], 'underp': j['prim_path']['underp']}
    with open(fname, "w") as f:
        f.write(json.dumps(new_j))
    return

if __name__ == '__main__':
    benchmark_table, resfile = iter_benchs.init ()
    for name in iter_benchs.names:
        source, path, is_rec = iter_benchs.get_info_from_name (benchmark_table, name)
        config_path = "{}/{}".format(path, "config.json")
        with open(config_path, "r") as f:
            j = json.load(f)
            print("{}: {} => {} => {}".format(config_path, j['method_predicates'], j['measure'], j["underp"]))
        # modify_file(config_path)
