import sys
import argparse
import os
import subprocess
verbose = True

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

workdir = ""

def invoc_cmd(cmd, output_file):
    if output_file is not None:
        # print("{}:{}".format(output_file, type(output_file)))
        if (verbose):
            print(" ".join(cmd + [">>", output_file]))
        with open(output_file, "a+") as ofile:
            try:
                subprocess.run(cmd, stdout=ofile)
            except subprocess.CalledProcessError as e:
                print(e.output)
    else:
        if (verbose):
            print(" ".join(cmd))
        try:
            subprocess.run(cmd)
        except subprocess.CalledProcessError as e:
            print(e.output)

if __name__ == '__main__':
    name = sys.argv[1]
    dir_str =name
    progfile = "{}/{}".format(dir_str, "prog.ml") 
    programs = [] 
    print(progfile)
    with open(progfile) as file:
        prog = ""
        progbegin = False 
        for line in file:
            if (line.strip () == "(*generated using Cobalt *)"): 
                continue
            elif ((" Program " in line.strip()) and (not progbegin)):  
                # print("Begin recording")
                progbegin = True
                prog = prog+"\n"+line.strip()
            elif ((" Program " in line.strip()) and (progbegin)):  
                programs.append(prog); 
                prog = line.strip()
            
            elif (line.strip() == ''):
                continue   
            else: 
                prog = prog+'\n'+line.strip()
                # print(line.rstrip())
        programs.append(prog)    
    i = 0    
    for progi in programs:
        i = i+1
        print (progi)
        ithprogrfile =  "{}/{}".format(dir_str, "prog"+(str(i))+".ml") 
        ithoutputfile = "{}/{}".format(dir_str, "prog"+(str(i))+".res")
        ifile = open (ithprogrfile, 'w')
        ifile.write(progi) 
        ifile.close()
        cmd = cmd_prefix + ["test", "under-type-check",
                            "{}/{}".format(dir_str, "config.json"),
                            ithprogrfile,
                            "{}/{}".format(dir_str, "_under.ml")]
        invoc_cmd(cmd, ithoutputfile)
