import json
import numpy as np
import matplotlib.pyplot as plt

dataset = 'u1base'

agdataO   = 'datasets/'+dataset+'O.agg'  # aggregated dataset with objects as keys
agdataF   = 'datasets/'+dataset+'F.agg'  # aggregated dataset with features as keys
bicdata  = 'biclusters/'+dataset+'.bic'  # Biclusters

fo = open(agdataO)
ff = open(agdataF)

dobj = {}
for i,l in enumerate(fo):
    dobj[l.split('\t')[0]]=i

dfeat = {}
for i,l in enumerate(ff):
    dfeat[l.split('\t')[0]]=i

n = max(dobj.values())+1
m = max(dfeat.values())+1

fulldata = np.zeros( (n,m) )

fo.seek(0)
for l in fo:
    obj, feats = l.rstrip().split('\t')
    i = dobj[obj]
    for f in feats.split(' '):
        j = dfeat[f]
        fulldata[i,j]=1

fo.close()
ff.close()

fb = open(bicdata)
for l in fb:
    bic = json.loads(l)
    slice_x = map( lambda x: dobj[x], bic['objs'] )
    slice_y = map( lambda x: dfeat[x], bic['feats'] )
    idx = np.ix_( slice_x, slice_y )
    vol = fulldata[idx].shape[0]*fulldata[idx].shape[1]
    print fulldata[idx].shape, len(fulldata[idx].nonzero()[0])/float(vol)
    #plt.imshow( fulldata[ idx ], cmap='Greys',  vmin=0 )
    #plt.show()

fb.close()
