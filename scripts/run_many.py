import sys
import argparse
import os
import subprocess
import shutil
verbose = True

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]
meta_config_file = "meta-config.json"
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


def run(dir_str):
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
    i = 0    
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
        invoc_cmd(cmd, ithoutputfile)
    (nr, ns) = generate_success_failed_numbers (outputdir)
    print ("Total "+str(i))
    print ("Rejected "+nr)
    print ("Succeeded "+ns)
    return str(i), nr, ns 
    


def generate_success_failed_numbers (d):
     
     cmd1 = ["grep", "-r", "type check failed", d]
     cmd2 = ["grep", "-r", "type check succeeded", d]
     try:
        ps = subprocess.run(cmd1, check=True, capture_output=True)
        rejected = subprocess.run(['wc', '-l'],
                              input=ps.stdout, capture_output=True)
        
        
        numrej = rejected.stdout.decode('utf-8').strip()

        ps = subprocess.run(cmd2, check=True, capture_output=True)
        accepted = subprocess.run(['wc', '-l'],
                              input=ps.stdout, capture_output=True)                      
        numsuc = accepted.stdout.decode('utf-8').strip()
        return (numrej, numsuc)                     
     except subprocess.CalledProcessError as e:
        print(e.output)


            
if __name__ == '__main__':
    name = sys.argv[1]
    dir_str =name
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
    i = 0    
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
        invoc_cmd(cmd, ithoutputfile)
    (nr, ns) = generate_success_failed_numbers (outputdir)
    print ("Total "+str(i))
    print ("Rejected "+nr)
    print ("Succeeded "+ns)
    [str(i), nr, ns]
