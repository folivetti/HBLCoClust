from numpy import argmin, array, mean, std, isnan
from numpy.random import randint, uniform
from collections import Counter
from itertools import izip
from scipy.stats import chisquare
from scikits.bootstrap import ci

from pylab import plot, show, savefig, close

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
        N = 100
        hash_list = []
        for l1, l2 in izip(f,f2):            
            features = l1.split()
            
            
            
            hashes = Counter(features)            
            hashes.update(l2.split())
            if len(set(features)) < N:
                hash_list.append(hashes.values()[:N])
            Xiter.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)
        '''
        hash_list = array(hash_list)
        avg_f = mean(hash_list,axis=0)        
        cfi = map(lambda x: ci(x) , hash_list.T)
        cfi = map(lambda x: x[1]-x[0], cfi)
        Viter = N*(std(avg_f) + 0.5*mean(cfi))
        print Viter
        '''

        XiterMean.append(mean(Xiter))
        XiterSTD.append(std(Xiter))

        hash_list = []
        f2 = open('results/doc1000rand'+str(i))
        for l1, l2 in izip(f,f2):
            features = l1.split()

            hashes = Counter(l1.split())
            hashes.update(l2.split())
            if len(set(features)) < N:
                hash_list.append(hashes.values()[:N])           
            Xrand.append(chisquare(hashes.values())[1])            
        f2.close()
        f.seek(0)

        XrandMean.append(mean(Xrand))
        XrandSTD.append(std(Xrand))
        '''
        hash_list = array(hash_list)
        avg_f = mean(hash_list,axis=0)
        cfi = map(lambda x: ci(x) , hash_list.T)
        cfi = map(lambda x: x[1]-x[0], cfi)
        Vrand = N*(std(avg_f) + 0.5*mean(cfi))
        print Vrand
        print ''
        '''
        #for u in range(2000):
        #    hashes = Counter(uniform(0,13795,i))
        #    Xuni.append(chisquare(hashes.values())[1])
                    
        plot(range(len(Xiter)),sorted(Xiter),'.',range(len(Xrand)),sorted(Xrand),'-.',range(len(Xrand)),array(range(len(Xrand)))/float(len(Xrand)),'*')
        savefig('doc'+str(i)+'.pdf', bbox_inches=0)
        close()

        #N = mean(bins)
        #avg_f = mean(hashes)
        #cfi = confidence(hashes)
        
        

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
