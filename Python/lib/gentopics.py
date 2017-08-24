from itertools import groupby, izip
from operator import itemgetter
from collections import Counter, defaultdict
import networkx as nx
import community as cmm
from math import log
import json

def gentopics(dataset):

    nbics = 0
    jointprob = defaultdict(int)
    indprob = defaultdict(int)
    with open('biclusters/'+dataset+'.bic') as f, open('topics/'+dataset+'.aggt','w') as fw, open('topics/'+dataset+'.aggw','w') as fw2:
        for l in f.readlines():
            jobj = json.loads(l)
            Objs = jobj['objs']
            Feats = jobj['feats']
            for o in Objs:
                fw.write('%s\t%s\n' % (o,' '.join(Feats)))
            for fe in Feats:
                fw2.write('%s\t%s\n' % (fe,' '.join(Objs)))
            for fe in Feats:
                indprob[fe] += 1
                for fe2 in Feats:
                    if fe < fe2:
                        jointprob[fe+' '+fe2] += 1
                    elif fe > fe2:
                        jointprob[fe2+' '+fe] += 1
            nbics += 1
    f.closed, fw.closed, fw2.closed

    # calculate mutual information
    mi = {}
    for k in jointprob:
        (f1,f2) = k.split()
        p11 = float(jointprob[k])/float(nbics)
        px1 = float(indprob[f1])/float(nbics)
        py1 = float(indprob[f2])/float(nbics)
        
        p00 = float(nbics - indprob[f1] - indprob[f2] + 2.0*jointprob[k])/float(nbics)
        p10 = float(nbics - indprob[f2] + jointprob[k])/float(nbics)
        p01 = float(nbics - indprob[f1] + jointprob[k])/float(nbics)
        

        
        px0 = 1.0 - px1
        
        py0 = 1.0 - py1

        eps = 1e-30

        Hx = -log(px0+eps,2)-log(px1+eps,2)
        Hy = -log(py0+eps,2)-log(py1+eps,2)

        
        mi[k] = 0.0
        mi[k] += p00*log(p00/(px0*py0+eps),2)
        mi[k] += p01*log(p01/(px0*py1+eps),2)
        mi[k] += p10*log(p10/(px1*py0+eps),2)
        mi[k] += p11*log(p11/(px1*py1+eps),2)
        
        #mi[k] = log(p11/(px1*py1),2)/(-log(p11,2))
        mi[k] *= 2.0/(Hx+Hy)
    print max(mi.values()),min(mi.values())
    sorted_mi = sorted(mi.iteritems(), key=itemgetter(1), reverse=True)

    newmi = sorted_mi.pop(0)
    groups = {}
    G = nx.Graph()
    while newmi[1] > 0.6:        
        (fe1,fe2) = newmi[0].split()
        G.add_edge(fe1,fe2)
        newmi = sorted_mi.pop(0)
    Gcc=nx.connected_component_subgraphs(G)
    G = Gcc[0]
    partition = cmm.best_partition(G)
        
    print 'topics = %d' % (max(partition.values()))
    print 'terms grouped = %d' % (len(G.nodes()))
    print 'terms not grouped = %d' % (len(indprob.keys()) - len(G.nodes()))
    countTopBics = 0
    with open('biclusters/'+dataset+'.bic') as f:
        for l in f.readlines():
            jobj = json.loads(l)
            Feats = jobj['hardcol'] + jobj['softcol']
            if any(x in G.nodes() for x in Feats):
                countTopBics += 1
    f.closed
    print countTopBics, nbics
    countTopDocs = 0
    ndocs = 0
    with open('datasets/'+dataset+'.agg') as f:
        for l in f.readlines():
            (did,Feats) = l.split('\t')
            Feats = Feats.split()
            if any(x in G.nodes() for x in Feats):
                countTopDocs += 1
            ndocs += 1
    f.closed
    print countTopDocs, ndocs

    groups = defaultdict(list)
    for k,v in partition.iteritems():
        px1 = float(indprob[k])/float(nbics)
        groups[v].append((k,-px1*log(px1,2)))
    fw = open('topics/'+dataset+'.twords','w')
    for k,v in groups.iteritems():
        fw.write( 'Topic %s:\n' % (k) )
        words = sorted(v, key=itemgetter(1), reverse=True)
        for (word,entropy) in words[:10]:
            fw.write( '\t%s %f\n' % (word,entropy) )
        fw.write('\n')
    fw.close()
        
    with open('topics/'+dataset+'.aggt') as f, open('topics/'+dataset+'.aggts','w') as fw:
        fw.writelines(sorted(f.readlines()))
    f.closed,fw.closed
    with open('topics/'+dataset+'.aggw') as f, open('topics/'+dataset+'.aggws','w') as fw:
        fw.writelines(sorted(f.readlines()))
    f.closed,fw.closed
    
    with open('topics/'+dataset+'.aggts') as f, open('topics/'+dataset+'.topic','w') as fw:
        lines = [line[:-1].split('\t') for line in f]
        for k,g in groupby(lines, key=itemgetter(0)):
            g = list(g)
            features = map(lambda x: x[1].split(), g )
            features = [item for sublist in features for item in sublist]
            cfeats = Counter(features)
            feats = []
            for w,c in cfeats.iteritems():
                feats.append('%s:%f' %(w,float(c)/float(len(g))))
            fw.write('%s\t%s\n\n' %(k,' '.join(feats)))
    f.closed,fw.closed

    with open('topics/'+dataset+'.aggws') as f, open('topics/'+dataset+'.words','w') as fw:
        lines = [line[:-1].split('\t') for line in f]
        for k,g in groupby(lines, key=itemgetter(0)):
            g = list(g)
            pt = float(len(g))/float(nbics)
            fw.write( '%s\t%s\n\n' %(k,-pt*log(pt)) )
    f.closed,fw.closed
