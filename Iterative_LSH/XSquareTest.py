from numpy import argmin, array, mean, std, isnan
from numpy.random import randint
from collections import Counter
from itertools import izip
from scipy.stats import chisquare


XiterMean = []
XrandMean = []

XiterSTD = []
XrandSTD = []

print 'Document test: '
print ''
with open('../../dadosdissertacaoR.agg') as f:
    for i in range(1000,11000,1000):
        Xiter = []
        Xrand = []
        
        f2 = open('results/doc1000iter'+str(i))
        for l1, l2 in izip(f,f2):
            features = l1.split()
            hashes = Counter(l1.split())
            hashes.update(l2.split())
            Xiter.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)

        XiterMean.append(mean(Xiter))
        XiterSTD.append(std(Xiter))

        f2 = open('results/doc1000rand'+str(i))
        for l1, l2 in izip(f,f2):
            features = l1.split()
            hashes = Counter(l1.split())
            hashes.update(l2.split())
            Xrand.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)

        XrandMean.append(mean(Xrand))
        XrandSTD.append(std(Xrand))
f.closed

print 'Random Hashes: '
for i,m,s in zip(range(1000,11000,1000),XrandMean,XrandSTD):
    print i,m,' +/- ',s
print ''

print 'Iterative Hashes: '
for i,m,s in zip(range(1000,11000,1000),XiterMean,XiterSTD):
    print i,m,' +/- ',s

# CHANGE
'''
XiterMean = []
XrandMean = []

XiterSTD = []
XrandSTD = []

print '\n\n'
print 'Integer test: '
print ''
with open('../../accidentsR.dat') as f:
    for i in range(1000,11000,1000):
        Xiter = []
        Xrand = []
        
        f2 = open('results/idx1000iter'+str(i))
        for l1, l2 in izip(f,f2):
            features = l1.split()
            hashes = Counter(l2.split())
            Xiter.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)

        XiterMean.append(mean(Xiter))
        XiterSTD.append(std(Xiter))

        f2 = open('results/idx1000rand'+str(i))
        for l1, l2 in izip(f,f2):
            features = l1.split()
            hashes = Counter(l2.split())
            Xrand.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)

        XrandMean.append(mean(Xrand))
        XrandSTD.append(std(Xrand))
f.closed

print 'Random Hashes: '
for i,m,s in zip(range(1000,11000,1000),XrandMean,XrandSTD):
    print i,m,' +/- ',s
print ''

print 'Iterative Hashes: '
for i,m,s in zip(range(1000,11000,1000),XiterMean,XiterSTD):
    print i,m,' +/- ',s
'''
