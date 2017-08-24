from collections import defaultdict
from itertools import combinations
import time

f = open('datasets/multi52.joined')

st = time.time()
count = defaultdict(int)
InvFeat = defaultdict(list)
docsId = {}

for l in f:
    oid, feat = l.rstrip().split('\t')
    count[feat] += 1
    InvFeat[feat].append(oid)
    docsId[oid]=1
f.close()

count = dict( (k,v) for k,v in count.iteritems() if v > 1 )
InvFeat = dict( (k, set(v)) for k,v in InvFeat.iteritems() )
print len(count.keys())

countDocs = {}
combs = 0
fw = open('resultsMulti5.txt','w')
for c in combinations(count.keys(),3):
    if (combs%100000000)==0:
        print combs/100000000, time.time()-st
    docs = InvFeat[c[0]] & InvFeat[c[1]] & InvFeat[c[2]] #reduce(lambda x, y: x&y, map(lambda x: InvFeat[x], c))
    combs += 1
    if len(docs) > 1 and all(len(docs) == count[x] for x in c):
        fw.write('_'.join(c) + '\t' + '_'.join(docs) + '\n')
        for d in docs:
            countDocs[d]=1
        '''
        for f in count.keys():
            if f!=c[0] and f!=c[1]:
                docs = docs & InvFeat[f]
                if len(docs) > 1 and all(len(docs) == count[x] for x in c):
                    fw.write('_'.join(c) + '_' + f + '\t' + '_'.join(docs) + '\n')
                    for d in docs:
                        countDocs[d]=1
        '''
fw.close()
st2 = time.time()

print st2-st, " seconds"
print float(len(countDocs.keys()))/float(len(docsId.keys()))
