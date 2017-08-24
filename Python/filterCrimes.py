import json
from collections import defaultdict

feat2bic = defaultdict(set)
biclen = {}
i=0

f = open('biclusters/crimeM.bic')
for l in f:
    bic = json.loads(l)
    if any(f[0]=='C' for f in bic['feats']):
        biclen[i] = len(bic['objs'])
        for f in bic['feats']:
            feat2bic[f.replace('@',' ')[1:]].add(i)
        i = i+1
fw = open('crimeTrained.bic','w')
fw.write(json.dumps(feat2bic))
fw.write('\n')
fw.write(json.dumps(biclen))
fw.close()

f.close()
