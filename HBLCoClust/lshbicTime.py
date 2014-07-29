import sys

from collections import defaultdict, Counter
import os
import cProfile
import json

from lib.utils import sortfile, link, gettime
from lib.coroutine import sink, sinkApp

import lib.AggregData as ad
import lib.lsh as lsh
import lib.bicluster as bic
import lib.networkbic as net
import lib.evaluate as ev
import lib.gentopics as gt
import lib.InClose2 as ic

import lib.postprocess as pp

# calculate the probability of a feature/object not ocurring
def calcProbability(data):
    
    probability = defaultdict(int)
    count = Counter() # count each feature frequency
    N = 0 # number of objects
    
    f = open(data)
    for l in f:
        k,vs = l.rstrip().split('\t')
        count.update(vs.split())
        N+=1
    f.close()

    invN = 1.0/float(N)
    for k,v in count.iteritems():
        probability[k] = 1.0 - (v*invN)

    return probability

def candstats( dataset, (do,df) ):
    f = open('candidates/'+dataset+'.cand')
    avgvol=0.0
    dobj={}
    dcol={}
    count=0
    for l in f:
        objs = l.rstrip().split('\t')
        objs, feats = objs[:-1], objs[-1].split('_')
        for o in objs:
            dobj[o]=1
        for fe in feats:
            dcol[fe]=1
        avgvol += float(len(objs)*len(feats))
        count+=1
    print avgvol/float(count), len(dobj), len(dcol)
    exclud = set(do.keys()) - set(dobj.keys())
    print exclud

    f.close()
    
def main(dataset, lshparams, (min_rows,min_cols), thr, sparse):

    # filenames for each step
    jdata    = 'datasets/'+dataset+'.data'   # original dataset in dyad format
    jdataI   = 'datasets/'+dataset+'I.data'  # inverted dataset in dyad format
    jdataIS  = 'datasets/'+dataset+'IS.data' # sorted inverted dataset in dyad format
    
    agdataO   = 'datasets/'+dataset+'O.agg'  # aggregated dataset with objects as keys
    agdataF   = 'datasets/'+dataset+'F.agg'  # aggregated dataset with features as keys

    lshdata  = 'lsh/'+dataset+'.lsh'         # LSH data
    slshdata = 'slsh/'+dataset+'.lsh'        # sorted LSH data
    candataU  = 'candidates/'+dataset+'.candU' # Biclusters Candidate
    candata  = 'candidates/'+dataset+'.cand' # Biclusters Candidate
    bicdata  = 'biclusters/'+dataset+'M.bic'  # Biclusters
    
    # link functions:
    # link( infile, function, outfile, parameters ):
    # infile => function(parameters) => outfile 

    # Pre-processing:
    
    # generate aggregated data
    
    link( jdata, ad.aggregate, agdataO )
    link( jdata, ad.invert, jdataI )
    sortfile( jdataI, jdataIS )
    link( jdataIS, ad.aggregate, agdataF )
    
    probabilityO = calcProbability(agdataO)
    probabilityF = calcProbability(agdataF)

    # remove unecessary data created, but preserve original dataset
    os.remove( jdataI )
    os.remove( jdataIS )
    
    # let's measure time taken at each step
    timer = gettime()

    # Algorithm:

    # Let's start working with the objects as keys:
    
    # generate lsh keys and sort by the generated hash key
    link( agdataO, lsh.genlsh, lshdata, (lshparams, probabilityO, thr,['']) )
    sortfile(lshdata,slshdata)        
    timer.send('LSH')
    
    # put lsh data into buckets
    link( slshdata, lsh.reducelsh, candataU )
    sortfile( candataU, candata )
    pp.mergecand( candata )
    timer.send('Reduce LSH')
    #candstats(dataset , bic.getinfo(agdataO))
    
    # generate biclusters
    link( candata, bic.genbic, bicdata, (bic.getinfo(agdataO), probabilityO, thr, (min_rows,min_cols), sparse, False) )
    timer.send('Gen. Bicluster')
   
    # Now do the same with the features as keys:
    
    # generate lsh keys and sort by the generated hash key
    link( agdataF, lsh.genlsh, lshdata, (lshparams, probabilityF, thr,['']) )
    sortfile(lshdata,slshdata)        
    timer.send('LSH')
    
    # put lsh data into buckets
    link( slshdata, lsh.reducelsh, candataU )
    sortfile( candataU, candata )
    pp.mergecand( candata )
    timer.send('Reduce LSH')
    #candstats(dataset , bic.getinfo(agdataO))
    
    # generate biclusters
    link( candata, bic.genbic, bicdata, (bic.getinfo(agdataO), probabilityO, thr, (min_rows,min_cols), sparse, True), append=True )
    timer.send('Gen. Bicluster')
    

    #pp.filterbics(bicdata)
    pp.merge(dataset)
    timer.send('merge')
    timer.close()
    #pp.hierclust(dataset,7)
    #pp.uncovered(dataset, bic.getinfo(agdataO))
    
    
if __name__ == "__main__":   
    
    nhashes,nkeys = 1000,3  # lsh parameters
    min_rows, min_cols = 4,4 # row, col thresholds
    thr = 0.0            # probability threshold
    sparse = 1.0            # sparseness rate    
    
    print 'House Votes 84 '
    dataset = 'house-votes-84'    # dataset name
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print
    
    print 'Zoo'
    dataset = 'zoo'    # dataset name
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print
    
    print 'Soybean Large'
    dataset = 'soybean-large'    # dataset name
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print
    
    print 'Soybean Small'
    dataset = 'soybean-small'    # dataset name
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    
    print 'Classic 3'
    dataset = 'classic3'
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print

    print 'Multi5'
    dataset = 'multi5'
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print

    print 'Multi 10'
    dataset = 'multi10'
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print

    print 'Movielens'
    dataset = 'u1baseTime'
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    print

