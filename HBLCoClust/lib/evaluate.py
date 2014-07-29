import json
from collections import defaultdict, Counter
import numpy as np
import math

def perplexity(dataset):
    pass

def PMI(dataset, prefix=''):
    f = open('datasets/'+dataset+'O.agg')
    cobjs = float(sum(1 for l in f))
    f.close()

    Pf = {}
    P = {}
    f = open('biclusters/'+dataset+prefix+'.bic')
    for l in f:
        feats = json.loads(l)['feats']
        for w1 in feats:
            Pf[w1] = 0
            if w1 not in P:
                P[w1] = {}
            for w2 in feats:
                if w1 != w2:
                    P[w1][w2] = 0
    f.close()
    
    f = open('datasets/'+dataset+'F.agg')
    cf = defaultdict(int)
    for l in f:
        word = l.split('\t')[0]
        if word in Pf:
            cf[ word  ] = len(l.split('\t')[1].split())
    f.close()
    
    tot = float( sum(cf.values()) )
    for k,v in cf.iteritems():
        Pf[k] = v/cobjs #tot

    f = open('datasets/'+dataset+'O.agg')
    count = 0
    for l in f:
        feats = l.rstrip().split('\t')[1].split()
        for w1 in feats:
            for w2 in feats:
                if w1 != w2 and w1 in P and w2 in P[w1]:
                    P[w1][w2] += 1
                    count += 1
    f.close()
    for w1 in P:
        for w2 in P[w1]:
            P[w1][w2] /= cobjs #float(count/2.0) #float(cf[w1] + cf[w2] - P[w1][w2])


    pmi = 0.0
    count = 0.0
    f = open('biclusters/'+dataset+prefix+'.bic')
    for l in f:
        feats = json.loads(l)['feats']
        for i,w1 in enumerate(feats):
            for w2 in feats[i+1:]:
                P[w1][w2] = 0.99999 if P[w1][w2]==1 else P[w1][w2]
                pmi -= math.log( P[w1][w2]/(Pf[w1]*Pf[w2]) , 2 )/math.log(P[w1][w2],2) if P[w1][w2] else 0.0
                count += 1
    f.close()

    print pmi/float(count)

# checar prob. conjunta
def NMI(dataset, prefix=''):

    # Probability of class
    f = open('datasets/'+dataset+'O.agg')
    ck = defaultdict(int)
    for l in f:
        oname = l.split('\t')[0]
        ck[ oname.split('.')[-1] ] += 1
    f.close()
    tot = float( sum(ck.values()) )
    Pk = {}
    for k,v in ck.iteritems():
        Pk[k] = v/tot

    # Probability of bicluster
    f = open('biclusters/'+dataset+prefix+'.bic')
    cB = defaultdict(int)
    for i,l in enumerate(f):
        cB[i] = len(json.loads(l)['objs'])
    f.close()
    Pb = {}
    tot = float(sum(cB.values()))
    for k,v in cB.iteritems():
        Pb[k] = v/tot

    # Joint probability of bicluster and class
    Pbk = {}
    sumB = 0.0
    f = open('biclusters/'+dataset+prefix+'.bic')
    for i,l in enumerate(f):
        Pbk[i] = defaultdict(float)
        for o in json.loads(l)['objs']:
            Pbk[i][o.split('.')[-1]] += 1
        sumB += sum(Pbk[i].values())
    for k in Pbk:
        for kl in Pbk[k]:
            Pbk[k][kl] /= float(sumB)
    f.close()
    
    nmi = 0.0
    Hb = -sum(map(lambda x: x*math.log(x,2), Pb.values()))
    Hk = -sum(map(lambda x: x*math.log(x,2), Pk.values()))

    for k1 in Pbk:
        for k2 in Pbk[k1]:
            try:
                nmi += Pbk[k1][k2]*math.log(Pbk[k1][k2]/(Pb[k1]*Pk[k2]),2)
            except:
                print Pbk[k1][k2], Pb[k1], Pk[k2]

    print 2*nmi/(Hb*Hk)    


def microprecision(dataset, prefix=''):
    f = open('biclusters/'+dataset+ prefix+ '.bic')
    microPrec = 0.0
    countBics = 0
  
    for l in f.readlines():

        jobj = json.loads(l)
        hardrow = jobj['objs']
        countBics += 1

        klasses = map( lambda x: x.split('.')[-1], hardrow )
        countPrec = Counter(klasses)
        microPrec += countPrec.most_common(1)[0][1] / float(sum(countPrec.values()))

    print microPrec/float(countBics)
    f.close()

def stats(dataset, prefix=''):
    f = open('biclusters/'+dataset+ prefix+ '.bic')
    countBics = 0
    countObjs = Counter()
    countFeats = Counter()

    rowSum = colSum = volSum = softRSum = softCSum = 0
    rowSize = []
    colSize = []
  
    for l in f:

        jobj = json.loads(l)
        Objs = jobj['objs']
        Feats = jobj['feats']
        countBics += 1
        rowSum += len(Objs)
        colSum += len(Feats)
        volSum += len(Objs)*len(Feats)

        rowSize.append(len(Objs))
        colSize.append(len(Feats))

        countObjs.update(Objs)
        countFeats.update(Feats)

    f.close()

    countTotObjs = Counter()
    countTotFeats = Counter()

    notInBic = {}
    
    f = open('datasets/'+dataset+'O.agg')
    for l in f:
        (oid,feat) = l.rstrip().split('\t')
        if oid not in countObjs:
            notInBic[oid]=1
        feats = feat.split()
        countTotObjs[oid]=1
        countTotFeats.update(feats)
    f.close()

    fw = open('notinbic.txt','w')
    for k in notInBic.keys():
        fw.write(k+'\n')
    fw.close()

    print '','-----------------------------'
    print 'From a total of %d biclusters: ' % (countBics)
    print '%.2f%% of objects (%d/%d)' % (100.0*(len(countObjs)/float(len(countTotObjs))),len(countObjs),len(countTotObjs))
    print '%.2f%% of features (%d/%d)' % (100.0*(len(countFeats)/float(len(countTotFeats))),len(countFeats),len(countTotFeats))
    print 'Average row size = %.2f +/- %.2f' % ( np.mean(rowSize), np.std(rowSize) )          #float(rowSum/countBics))
    print 'Average column size = %.2f +/- %.2f' % ( np.mean(colSize), np.std(colSize) )#(float(colSum/countBics))
    print 'Average volume = %.2f' %  (float(volSum/countBics))
    print '-----------------------------',''
