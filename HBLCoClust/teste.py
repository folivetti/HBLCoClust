from numpy import argmin, array, mean, std, isnan
from numpy.random import randint
from collections import Counter
from scipy.stats import chisquare

P = 109297

for nhashes in range(100,1100,100):

    print "N = ", nhashes
    
    f = open('datasets/RUSERS.agg')
    X = []
    for line in f:
        (obj, feats) = line[:-1].split('\t')
        feats = feats.split()
        h = map( lambda feat: hash(feat), feats)
        mhs = []
        for i in range(nhashes):
            a, b = randint(1,P), randint(0,P)
            nh = map( lambda x: (a*x+b)%P, h )
            mhs.append(argmin(nh))
        c = Counter(mhs)
        
        X.append(chisquare(c.values())[1])

    f.close()
    X = filter(lambda x: isnan(x)==False, X)
    print "original: ",mean(X), std(X)

    f = open('datasets/classic3.agg')
    X = []
    for line in f:
        (obj, feats) = line[:-1].split('\t')
        feats = feats.split()
        h = map( lambda feat: hash(feat), feats)
        mhs = []
        for i in range(nhashes):
            nh = map( lambda x: (200*x + (11+x)*i)%P, h )
            mhs.append(argmin(nh))
        c = Counter(mhs)
        
        X.append(chisquare(c.values())[1])

    f.close()
    X = filter(lambda x: isnan(x)==False, X)
    print "simplified: ",mean(X), std(X)
