from itertools import groupby, izip
from operator import itemgetter
from coroutine import coroutine

# convert a dyad data into aggregated data
@coroutine
def aggregate(target):
    current_key = ''
    features = []
    try:
        while True:
            line = (yield)
            try:
                k,t = line.rstrip().split('\t')
            except:
                print line
            if k==current_key:            
                features.append(t)            
            else:
                if len(features)>0:
                    target.send( '%s\t%s\n' % (current_key, ' '.join(features)) )
                features = [t]
                current_key = k
    except GeneratorExit:
        if len(features)>0:
            target.send('%s\t%s\n' % (current_key, ' '.join(features)) )

@coroutine
def invert(target):
    while True:
        line = (yield)
        k,v = line.rstrip().split('\t')
        target.send('%s\t%s\n' % (v,k))
