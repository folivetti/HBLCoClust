import json
import numpy as np

f = open('biclusters/spectraInClose.bic')
lines = f.readlines()
f.close()

vol = np.zeros( len(lines) )

for i, l in enumerate(lines):
    data = json.loads(l)
    volume = (len(data['feats'])*len(data['objs']))
    vol[i] = volume

fw = open('biclusters/spectraInCloseSorted.bic', 'w')
for i in np.argsort(vol)[::-1]:
    fw.write(lines[i])
fw.close()
