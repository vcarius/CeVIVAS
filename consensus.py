#!/usr/bin/env python

import sys
from Bio import AlignIO
from Bio.Align import AlignInfo

alignment = AlignIO.read(sys.argv[1], 'fasta')
summary_align = AlignInfo.SummaryInfo(alignment)
consensus = summary_align.dumb_consensus(float(sys.argv[2]))

print(">consensus\n"+str(consensus))

