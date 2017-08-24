from collections import Counter
from math import log
import json
from itertools import izip
import sys
import getopt
import networkx as nx
from operator import itemgetter
from random import random, shuffle, randint

def geragraph(fname,bics,countbic, G, Gatt):
    
    
    maxb = float(bics[0][1])
    for b in bics:
        if b[1] > maxb:
            maxb = float(b[1])

    countedges = 0
    discarded = 0

    nbics = len(bics)
    for b in bics:
        bc = b[0]
        f = ' '.join(b[2])
        
        #shuffle(bc)
        for b1 in bc:
            if len(b1.split(' '))>1:
                classe = b1.split(' ')[1]
                G.add_node(b1,classe=classe)
            for b2 in bc:
                if len(b2.split(' '))>1:
                    classe = b2.split(' ')[1]
                    G.add_node(b2,classe=classe)
                if b1!=b2:
                    if G.has_edge( b1, b2 ):
                        weight = G[b1][b2]['weight']+1
                        words = G[b1][b2]['words']+f
                        G.add_edge(b1,b2,{'words':words,'weight':weight})
                    else:
                        G.add_edge(b1,b2,{'words':f, 'weight':1})

        for w1 in b[2]:
            for w2 in b[2]:
                if not (Gatt.has_edge(w1,w2) and Gatt.has_edge(w2,w1)):
                    Gatt.add_edge(w1,w2)
    nx.write_gexf(G, 'graphs/'+fname+'.gexf')
    nx.write_gexf(Gatt, 'graphs/'+fname+'_att.gexf')
    
def readbic(fname):
    
    f = open(fname)
    lines = f.readlines()
    f.close()

    biclusters = []
    for l in lines:
        jobj = json.loads(l)
        bics = jobj['objs']
        lbic = len(bics)
        feats = jobj['feats']
        biclusters.append( (bics,lbic,feats) )

    biclusters = sorted(biclusters,key=itemgetter(1))
    
    return biclusters

def geracounter(biclusters):
    countbic = Counter()
    for b in biclusters:
        for objs in b[0]:
            countbic[objs] += 1
    return countbic

def generatenodes(fname, countbic, G):
    f = open('datasets/'+fname+'O.agg')
    for l in f.readlines():        
        (oid,feats) = l[:-1].split('\t')
        if oid in countbic:
            G.add_node(oid)
    f.close()
        
def genetwork(dataset):

    G = nx.Graph()
    Gatt = nx.Graph()

    biclusters = readbic('biclusters/'+dataset+'.bic')
    countbic = geracounter(biclusters)
    generatenodes(dataset, countbic, G)
    geragraph(dataset,biclusters,countbic, G, Gatt)
