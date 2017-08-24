# -*- coding: cp1252 -*-
from collections import defaultdict, Counter
from copy import deepcopy
import json
from lib.coroutine import coroutine
import operator
import numpy as np
import re

# melhorar a interface desse
def getinfo(agdata):
    drows = {}
    dcols = defaultdict(set)    
    with open(agdata) as f:
        for line in f:
            (obj, feats) = line.rstrip().split('\t')
            fs = feats.split()
            drows[obj] = set(fs)
            for ff in fs:
                dcols[ff].add(obj)
    f.closed
    return drows,dcols

def testsets(hardrow,hardcol,drows,dcols):
    if set(hardrow) <= reduce(lambda x,y: x & y, map(lambda x: dcols[x],hardcol)) and set(hardcol) <= reduce(lambda x,y: x & y, map(lambda x: drows[x],hardrow)):
        return True
    return False

@coroutine
def genbic(target,((drows,dcols), prob, thr, (min_rows,min_cols), sparse, reverse)):

    sortedFeats = map(lambda x: x[0],sorted(prob.iteritems(), key=operator.itemgetter(1), reverse=True))
    
    bicList = {}
    candList = {}
    rowList = {}
    colList = {}
    countBic=0
    while True:
        line = (yield)
        objs = line.rstrip().split('\t')
        objs, feats = objs[:-1], objs[-1].split('_')

        if reverse:
            objs, feats = feats, objs
                
        countFeats = Counter()
        countObjs = Counter()

        # vertical bicluster
        map( lambda x: countObjs.update(dcols[x]), feats )
        vobjs = [k for k,v in countObjs.iteritems() if v==len(feats)]#==len(feats)]

        # horizontal bicluster       
        map( lambda o: countFeats.update(drows[o]), objs )
        hfeats = [f for f in sortedFeats if countFeats[f]==len(objs) and prob.get(f,0.0)>=thr] #==len(objs)
      
        if len(vobjs) < min_rows or len(hfeats) < min_cols: # |vobjs| and |hfeats| represent the maximum bicluster size possible
            continue

        bid = hash(''.join(sorted(vobjs)+sorted(hfeats)))
        if bid in candList:
            continue
        candList[bid]=1

        if all(r in rowList for r in vobjs) and all(c in colList for c in hfeats):
            continue
        else:
            rowList.update( [(r,1) for r in vobjs] )
            colList.update( [(c,1) for c in hfeats] )

        # In-Close2:        

        # create binary data matrix D
        D = np.zeros( (len(vobjs),len(hfeats)) )
        for i,o in enumerate(vobjs):
            D[i,[j for j,f in enumerate(hfeats) if f in drows[o]]] = 1

        Stack = [( set(range(D.shape[0])), set(),0 )] # stack of candidates Ar, Br and y
        while len(Stack):
            (A,B,y) = Stack.pop(0)
            J = []
            An = []
            for j in range(y,D.shape[1]):
                if j not in B:
                    RW = A & set(D[:,j].nonzero()[0])
                    if len(RW) >= sparse*len(A):
                        B.add(j)
                    elif len(RW)>=min_rows and all(D[list(RW),k].sum()<sparse*len(RW) for k in set(range(j)) - B):  # is RW canonical?
                        J.append(j)
                        An.append(RW)
            # dump bicluster found
            countFeats2 = Counter()
            countObjs2 = Counter()
            hardcol = [hfeats[i] for i in B]
            map( lambda x: countObjs2.update(dcols[x]), hardcol )                
            hardrow = [unicode(k, errors='ignore') for k,v in countObjs2.iteritems() if v>=sparse*len(hardcol)]
            map( lambda o: countFeats2.update(drows[o]), hardrow )
            hardcol = [unicode(k, errors='ignore') for k,v in countFeats2.iteritems() if v>=sparse*len(hardrow) and prob.get(k,0.0)>=thr]
           
            if len(hardrow) >= min_rows and len(hardcol) >= min_cols:
                
                #hardrow = [vobjs[i] for i in A]                
                #hardcol = [hfeats[i] for i in B]
                bid = hash(''.join(sorted(hardrow)+sorted(hardcol)))
                if bid not in bicList:
                    bicList[bid] = 1
                    countBic += 1
                    bicluster = {"objs":hardrow,"feats":hardcol}
                    try:
                        s = json.dumps(bicluster) + '\n'
                    except:
                        print bicluster
                    target.send( json.dumps(bicluster) + '\n' )

            # stack all remaining candidates
            for y,A in zip(J,An):
                Bn = B | set([y])
                Stack.append((A,Bn,y))
