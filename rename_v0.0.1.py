import os
import argparse


def main():
    
    parser = argparse.ArgumentParser(description='rename',     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--input', required=True, type=str, help='A .csv file with address column', default=None)
    parser.add_argument('--list', required=True, type=str, help='A .csv file with address column', default=None)
    parser.add_argument('--output', required=True, type=str, help='Output file name', default=None)
    
    args = parser.parse_args()
    
    print(args)
    
    pattern={}

    with open(args.list, "r") as list:
        
        #lines = list.readlines()
        for line in list:
            line=line.rstrip()
            aux= line.split("\t")
            pattern[aux[0]]=aux[1]
    print(pattern)
    with open(args.input) as file:
        output=open(args.output, "w")
        #lines = list.readlines()
        for line in file:
            line = line.rstrip()
            #FLAG=0
            if line.find(">") != -1:
                try:
                    aux=line.split(">")[1]
                    output.write(">"+pattern[aux])
                except:
                    print("FOUND AN ERROR WITH "+line+"\n")
                    output.write(line)
            else:
                output.write(line)

            output.write("\n")
        output.close()

if __name__ ==  "__main__":

    main()

    exit()
