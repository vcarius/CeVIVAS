#!/usr/bin/env python
import json
import argparse
from Bio import SeqIO
from Bio import AlignIO
from Bio.Align import AlignInfo
import subprocess

def _CONSENSUS_CREATE (ALN_FILE=None, THRESHOLD=0.51):
    
    PREFIX=ALN_FILE.rsplit(".", 1)[0]
    OUTPUT_FILE=PREFIX+"_CONSENSUS.fasta"
    alignment = AlignIO.read(ALN_FILE, 'fasta')
    summary_align = AlignInfo.SummaryInfo(alignment)
    consensus = summary_align.dumb_consensus(float(THRESHOLD), ambiguous='?')
    OUT = open(OUTPUT_FILE, "w")
    OUT.write(">"+str(PREFIX)+"\n")
    OUT.write(str(consensus.upper())+"\n")
    OUT.close()

def _RUN_MAFFT(FASTA_FILE=None, OUTPUT_FILE=None, MAFFT_PARAMETER=None):
    
    if MAFFT_PARAMETER:
        COMMAND= "mafft --thread 1"+MAFFT_PARAMETER+" "+FASTA_FILE+" > "+OUTPUT_FILE
        result = subprocess.run(COMMAND, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True) 
    else:
        COMMAND= "mafft --auto --thread 1 "+FASTA_FILE+" > "+OUTPUT_FILE
        result = subprocess.run(COMMAND, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    
    print(str(result.stderr))
    print(str(result.stdout))        
    
    return result

def CONSENSUS(FASTA_FILE=None, MAFF_PARAMETER=None, THRESHOLD=0.51):
    if FASTA_FILE:
        PREFIX=FASTA_FILE.rsplit(".", 1)[0]
        OUTPUT_FILE=PREFIX+".aln"
        
        result = _RUN_MAFFT(FASTA_FILE=FASTA_FILE, OUTPUT_FILE=OUTPUT_FILE, MAFFT_PARAMETER=MAFF_PARAMETER)

        if result.returncode:
        
            print("ERROR: An unexpected error occurred. Check MAFFT parameters or whether the program is installed on your system")
        
        else:
            _CONSENSUS_CREATE(ALN_FILE=OUTPUT_FILE, THRESHOLD=THRESHOLD)

def LOAD_JSON(JSON_FILE=None):
    if JSON_FILE:
        with open(JSON_FILE, 'r') as json_file:
            json_data = json.load(json_file)

        return json_data
    else:
        print("ERROR:\n"+str(JSON_FILE)+"JSON FILE NOT FOUND\n")

def SAVE_JSON(JSON_FILE=None, JSON=None):
    try:
        with open(JSON_FILE, 'w', encoding='utf-8') as json_file:
            json.dump(JSON, json_file, ensure_ascii=False, indent=4)
    except:
        print("ERROR:\n It was not possible save"+JSON_FILE+". Check if you have permission to write to the folder\n")

def JSON_CHECK(JSON=None):
    if 'tree' not in list(JSON.keys()):
        print("ERROR:\n JSON has not tree key. Check if you current JSON FILE is right.\n")
        return 0
    else:
        for key in ['name', 'node_attrs', 'branch_attrs', 'children']:
            if key not in list(JSON['tree'].keys()):
                print("ERROR:\n JSON has not "+key+" key. Check if you current JSON FILE is right.\n")
                return 0
            elif key == 'node_attrs':
                if 'clade_membership' not in list(JSON['tree'].keys()):
                    print("ERROR:\n JSON has not clade_membership key. Check if you current JSON FILE is right.\n")
                    return 0
            else:
                return 1
            
def LOAD_FASTA(FASTA_FILE=None):

    try:
        FASTA_SEQS = {}
        for s in SeqIO.parse(FASTA_FILE, "fasta"):
            ID = s.description
            SEQ = str(s.seq)

            FASTA_SEQS[ID]=SEQ

        return FASTA_SEQS

    except:
        print("WARNING:\n It was not possible load"+FASTA_FILE+".\n")

def SAVE_FASTA(FASTA_DICT=None, LIST_NAMES=None, OUTPUT_FILE=None):
    try:

        OUTPUT=open(OUTPUT_FILE, "w")

        for k in list(FASTA_DICT.keys()):
            ID = k.split("|")[0]
            SEQ=FASTA_DICT[k]

            if ID in LIST_NAMES:
                OUTPUT.write(">"+str(k)+"\n")
                OUTPUT.write(str(SEQ)+"\n")
        
        OUTPUT.close()

        return True

    except:
        print("WARNING:\n It was not possible save "+OUTPUT_FILE+". Check if you have permission to write to the folder\n")

def FindNode(JSON=None, Node=None):
    node_name = JSON["name"]

    if node_name == Node:
        return JSON

    else:

        if "children" in list(JSON.keys()):
            for sjon in JSON["children"]:
                Branch = FindNode(JSON=sjon, Node=Node)
                if Branch:
                    return Branch


def FindClade(JSON=None, CLADE=None):
    membership = JSON["node_attrs"]["clade_membership"]["value"]

    if membership == CLADE:
        return JSON

    else:

        if "children" in list(JSON.keys()):
            for sjon in JSON["children"]:
                Branch = FindClade(JSON=sjon, CLADE=CLADE)
                if Branch:
                    return Branch

def getNames(LIST=[], JSON=None):
    if 'children' in list(JSON.keys()):
        for sjon in JSON["children"]:
            getNames(LIST=LIST, JSON=sjon)
    else:
        LIST.append(JSON["name"])


def main():
    
    parser = argparse.ArgumentParser(description='RetrieveFasta.py is a script designed to obtain fasta sequences of members from a selected clade on final .json file created by nextstrain workflow.')
    parser.add_argument('--json_file', help='Final JSON FILE obtain from NEXTSTRAIN workflow', default=None, required=False, type=str)
    parser.add_argument('--fasta_file', help='FASTA FILE used as input to NEXTSTRAIN workflow', required=True, type=str)
    parser.add_argument('--only_consensus', help='', required=False, default=False, type=bool)
    #parser.add_argument('--clade', help='Name (code) of desired clade(s). This option is predominant over --node_name', nargs='+', required=False, default=None, type=str)
    #parser.add_argument('--node_name', help='Name (code) of desired tree node', nargs='+', required=False, default=None, type=str)
    parser.add_argument('--id_sequence', help='Name (code) of desired sequence(s)', nargs='+', required=False, default=None, type=str)
    #parser.add_argument('--node_attrs', help='Name (code) of node attribute. Accepted values: div, num_date, region, haplotype, confidence, lbi, glyc, or clade_membership', nargs='+', required=False, default=None, type=str)
    #parser.add_argument('--branch_attrs', help='Name (code) of branch attribute. Accepted values: mutations or label', nargs='+', required=False, default=None, type=str)
    parser.add_argument('--mafft_parameter', help='', required=False, default=None, type=str)
    parser.add_argument('--consensus_threshold', help='', required=False, default=0.51, type=float)
    parser.add_argument('--output_prefix', help='PREFIX NAME of output files (NOT REQUIRED EXTENSION)', required=False, default="OUTPUT", type=str)
    group = parser.add_mutually_exclusive_group(required=False)
    group.add_argument('--clade', help='Name (code) of desired clade(s).', nargs='+', required=False, default=None, type=str)
    group.add_argument('--node_name', help='Name (code) of desired tree node.', nargs='+', required=False, default=None, type=str)
    args = parser.parse_args()
    print(args)
    
    if args.json_file is None and args.only_consensus is False:
        print("Not passed json.file !!!")
        exit()
        
    elif args.only_consensus is True:
        CONSENSUS(FASTA_FILE=args.fasta_file, MAFF_PARAMETER=args.mafft_parameter, THRESHOLD=args.consensus_threshold)
        exit()
        
    #LOAD JSON
    JSON=LOAD_JSON(JSON_FILE=args.json_file)
    JSON_CHECK(JSON=JSON)
    JSON=JSON['tree']
    JSON_DICT={}

    #LOAD FASTA
    FASTA_DICT = LOAD_FASTA(FASTA_FILE=args.fasta_file)

    if args.clade:
        for clade in args.clade:
            #FIND CLADE
            NEW_JSON = FindClade(JSON=JSON, CLADE=clade)
    
            if not NEW_JSON:
            
                print("It was not found members to "+clade+"\n")
                #exit()

            else:
                JSON_DICT[clade]=NEW_JSON

    elif args.node_name:
        for node in args.node_name:
            #FIND CLADE
            NEW_JSON = FindNode(JSON=JSON, Node=node)
    
            if not NEW_JSON:
            
                print("It was not found members to "+node+"\n")
                #exit()

            else:
                JSON_DICT[node]=NEW_JSON
    
    else:
        JSON_DICT['ALL']=JSON

    #SAVE JSON
    for k in list(JSON_DICT.keys()):
        SAVE_JSON(JSON_FILE=args.output_prefix+"_"+str(k)+".json", JSON=JSON_DICT[k])

    #GET NAME OF LEAVES
    NAMES_DICT={}
    
    for k in list(JSON_DICT.keys()):
        NAMES=[]
        getNames(LIST=NAMES, JSON=JSON_DICT[k])
        NAMES_DICT[k]=NAMES

    if args.id_sequence:
        IDs=list(args.id_sequence)
        #print(IDs)
        for k in list(NAMES_DICT.keys()):
            UPDATED = list(set(NAMES_DICT[k]) & set(IDs))
            NAMES_DICT[k]=UPDATED
      
    #PARSE AND CREATE FASTA FILE
    #PARSE AND CREATE A CONSENSUS FASTA FILE FOREACH SELECTED CLADE OR NODE
    for k in list(NAMES_DICT.keys()):
        returncode = SAVE_FASTA(FASTA_DICT=FASTA_DICT, LIST_NAMES=NAMES_DICT[k], OUTPUT_FILE=args.output_prefix+"_"+str(k)+".fasta")
        
        if returncode:
            CONSENSUS(FASTA_FILE=args.output_prefix+"_"+str(k)+".fasta", MAFF_PARAMETER=args.mafft_parameter, THRESHOLD=args.consensus_threshold)
        else:
            print("WARNING:\n It was not possible save a consensus fasta file to "+args.output_prefix+"_"+str(k)+".fasta"+".\n")

    print("\n\nCONGRATULATIONS !!!!!!!!!!!\nWe finished without errors. Check the output files "+args.output_prefix+"*.{json,fasta}")
    print("Thanks a lot !!!}")

    #else:
    #    print("\n\nSorry!!!\nWe was not able to save the file "+args.output_prefix+".fasta")
    #    print("Check all inputs.\n}")

if __name__ == "__main__":
    
    main()
    exit()
