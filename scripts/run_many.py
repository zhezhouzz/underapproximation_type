import sys
import argparse
import os
import subprocess
import shutil
verbose = True

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]
meta_config_file = "meta-config.json"
workdir = ""



def parse_stat (resfile):
    line = None
    with open (resfile) as f:
        line = f.readline().split('&')
        line = [elem.replace("\n", "").replace("$", "").replace(" ", "") for elem in line]
    return line

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


def run(dir_str):
    resfile = ".result"
    progfile = "{}/{}".format(dir_str, "prog.ml") 
    inputdir = "{}/{}".format(dir_str, "inputs")
    outputdir = "{}/{}".format(dir_str, "outputs")

    if os.path.exists(outputdir):
        shutil.rmtree(outputdir)
        os.makedirs(outputdir)
    else:
        os.makedirs(outputdir)

    if os.path.exists(inputdir):
        shutil.rmtree(inputdir)
        os.makedirs(inputdir)
    else:
        os.makedirs(inputdir)
        
        
    programs = [] 
    result = [] #[Total, #Complte]
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
    i, successful, failed = 0,0,0
    for progi in programs:
        i = i+1
        print (progi)
        # ithprogrfile =  "{}/{}/{}".format(dir_str, "prog"+(str(i))+".ml") 
        ithprogrfile =  "{}/{}".format(inputdir, "prog"+(str(i))+".ml") 
        ithoutputfile = "{}/{}".format(outputdir, "prog"+(str(i))+".res")
        ifile = open (ithprogrfile, 'w')
        ifile.write(progi) 
        ifile.close()
        cmd = cmd_prefix + ["coverage-type-check", meta_config_file,
                            # "{}/{}".format(dir_str, "config.json"),
                            ithprogrfile,
                            "{}/{}".format(dir_str, "_under.ml")]
        # invoc_cmd(cmd, ithoutputfile)
        invoc_cmd(cmd, None)
        res = parse_stat (resfile)
        if res[0] == "true":
            successful + 1
        else:
            failed = failed + 1
        
    print ("Total "+str(i))
    print ("Rejected "+str(failed))
    print ("Succeeded "+str(successful))
    return str(i), str(failed), str(successful) 
    
