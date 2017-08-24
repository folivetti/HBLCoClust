from collections import Counter
import json

labels = {}
f2 = open('/home/olivetti/Downloads/spectraChart26.data.map')
for l in f2:
    fields = l.strip().split(':')
    labels[fields[0]] = ':'.join(fields[1:])
f2.close()

score = {}
f1 = open('/home/olivetti/Downloads/vol/tmp/heidensi/d4j4/Chart/26/.BugLoRD_data/ranking/tarantula/ranking.rnk')
for l in f1:
    fields = l.split(':')
    id = ':'.join(fields[:-1])
    tarantula = float(fields[-1])
    score[id] = tarantula
f1.close()

f = open('biclusters/spectraChart26_3InClose.bic')
i=0

allO = []

for l in f:
    bic = json.loads(l)

    allO = allO + bic['objs']*len(bic['feats'])

cO = Counter(allO)
i = 1
for o, v in cO.most_common():
    print i, labels[o]
    i = i+1

f.close()
