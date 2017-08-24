from collections import defaultdict
from math import log

def calcEntropy(data):
    ent = defaultdict(int)
    cont = defaultdict(int)
    N = 0
    oldk = ''
    
    f = open(data)
    for l in f:
        k,v = l[:-1].split('\t')
        cont[v] += 1
        if k != oldk:
            N+=1
            oldk = k
    f.close()

    logN = log(float(N))
    invN = 1.0/float(N)
    for k in cont.keys():
        pi1 = cont[k]
        pi0 = N - cont[k]
        if pi0<=0 or pi1 <= 0:
            ent[k] = 0.0
        else:
            try:
                ent[k] = pi0*invN#logN - invN*(pi0*log(pi0) + pi1*log(pi1))
            except:
                print pi0,pi1

    return ent, cont

objTarget = '38859 comp.graphics'

data = {}
f = open('datasets/multi5.agg')
for l in f:
    oid,feats = l.rstrip().split('\t')
    feats = feats.split()
    data[oid] = set(feats)
f.close()

ent, cont = calcEntropy('datasets/multi5.joined')

for o in data.keys():
    if o != objTarget:
        terms = data[o] & data[objTarget]
        filteredTerms = map(lambda x: (x,cont[x],round(ent[x],2)), filter( lambda x: cont[x]<=20, terms ))
        if len(filteredTerms)>=2:
            print o, filteredTerms
        
# verify other objects with +/- 5 features in common at least
# verify if they form a bicluster
