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
    
    
if __name__ == "__main__":

    # HEY LISTEN!
    # FILTER BICLUSTERS BY SORTING THEM AND SELECTING
    # THOSE THAT COVER EVERYTHING FOUND
    # AFTER THAT, APPLIES HIERARCHICAL CLUSTERING TO
    # DEFINE MORE INFORMATIVE AND LARGE CLUSTERS
   
    dataset = 'artur'    # dataset name
    nhashes,nkeys = 2000,3  # lsh parameters
    min_rows, min_cols = 50,4 # row, col thresholds
    thr = 0.2            # probability threshold
    sparse = 0.5            # sparseness rate    
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    ''' 
    dataset = 'DB'    # dataset name
    nhashes,nkeys = 1000,3  # lsh parameters
    min_rows, min_cols = 5,5 # row, col thresholds
    thr = 0.95            # probability threshold
    sparse = 0.5            # sparseness rate    
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    
    dataset = 'Yahoo'    # dataset name
    nhashes,nkeys = 2000,3  # lsh parameters
    min_rows, min_cols = 5,5 # row, col thresholds
    thr = 0.95            # probability threshold
    sparse = 0.5            # sparseness rate
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    
    dataset = 'YelpBin'    # dataset name
    nhashes,nkeys = 2000,3  # lsh parameters
    min_rows, min_cols = 5,5 # row, col thresholds
    thr = 0.95            # probability threshold
    sparse = 0.5            # sparseness rate
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    
    dataset = 'YelpFull'    # dataset name
    nhashes,nkeys = 2000,3  # lsh parameters
    min_rows, min_cols = 5,5 # row, col thresholds
    thr = 0.95            # probability threshold
    sparse = 0.5            # sparseness rate
    main(dataset, (nhashes,nkeys), (min_rows,min_cols), thr, sparse)
    '''
