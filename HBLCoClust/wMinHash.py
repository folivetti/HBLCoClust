from collections import defaultdict
from math import floor, log, exp
from random import random, gammavariate, seed, normalvariate
k = 11

def wJaccard(o1,o2):
    do1 = dict(o1)
    do2 = dict(o2)
    keys = set(do1.keys()) | set(do2.keys())
    num = 0.0
    den = 0.0
    for k in keys:
        v1 = do1.get(k,0.0)
        v2 = do2.get(k,0.0)
        num += min(v1,v2)
        den += max(v1,v2)
    return num/den

def estJaccard(h1, h2):
    J = 0.0
    for t in zip(h1,h2):
        if t[0] == t[1]:
            J += 1
    
    return J/float(len(h1))

mhashes = defaultdict(lambda: [])
data = defaultdict(lambda: [])

f = open('base.txt')
for ln, l in enumerate(f):
    for i in l.split():
        i,v = i.split(',')
        idx,v = int(i), float(v)
        data[ln].append( (idx,v) )
f.close()

for it in range(1000):
    r = []
    c = []
    beta = []
    tot = []
    for i in range(k):
        #r.append(gammavariate(2,1))
        seed(hash('atrib')+i+it)
        r.append(-log(random())-log(random()))
        c.append(log(-log(random())-log(random())))
        beta.append(random())
        tot.append(log(-log(random())-log(random())) - random()*(-log(random())-log(random())) - normalvariate(0,1))

    f = open('base.txt')
    for ln, l in enumerate(f):
        mina = float('Inf')
        mink = k+1
        mint = float('Inf')
        for i in l.split():
            i,v = i.split(',')
            idx,v = int(i), float(v)        
            t = floor(log(v)/r[idx] + beta[idx])
            y = exp(r[idx]*(t-beta[idx]))
            a = c[idx]/(y*exp(r[idx]))

            t = floor(log(v)/r[idx] + beta[idx])
            a = c[idx] - r[idx]*(1 + t - beta[idx])

            a = c[idx] - log(v)

            #a = tot[idx] - log(v)
            
            if a < mina:
                mina = a
                mink = idx
        mhashes[ln].append( mink )
    f.close()
   
#for k,v in mhashes.iteritems():
#    print k,v

for k1 in data.keys():
    for k2 in data.keys():
        if k1 != k2:
            print wJaccard(data[k1],data[k2]), estJaccard(mhashes[k1],mhashes[k2])
