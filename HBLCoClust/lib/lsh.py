from coroutine import coroutine
import time
from numpy import array, vectorize, remainder, argmin, where
from numpy.random import  randint

@coroutine
def genlsh( target,((nhs,nhashes), prob,thr, allowed) ):

    PRIME = 109297#7577#109297        # choose a large prime number
    nlisthashes = nhashes*nhs  # total number of numbers to be generated

    # generate two random values to create a series of pseudo-random permutations
    a = randint(low=1,high=PRIME-1, size=nlisthashes)
    b = randint(low=1,high=PRIME-1, size=nlisthashes)

    maphash = vectorize(hash)
    minhashes = ['']*nlisthashes # pre-allocate list of hash keys
    
    while True:
        line = (yield)
        (obj,feats) = line.rstrip().split('\t')
        feats = feats.split()

        feats = filter(lambda x: prob[x]>=thr and any( f in x for f in allowed ),feats)#[f for f in feats if prob[f]>=thr]

        # check if there are enough features to generate the hash keys
        if len(feats)>=nhashes:

            h = maphash(feats)
            dh = dict( enumerate(feats) )
                       
            # iterative minhash procedure as described in: [1]
            #z1 = a*h
            #z2 = b+h

            for i in range(nlisthashes):
                z1 = remainder(a[i]*h + b[i], PRIME)#remainder(z1+z2,PRIME) 
                minhashes[i] = dh[argmin(z1)] # get the feature with the min hash value

            # group permutated features in nhashes sized groups
            lshs = ( '%s\t%s\n' % ('_'.join( minhashes[i:i+nhashes] ),obj) for i in range(0,nlisthashes-nhashes+1,nhashes))
            target.send(lshs)
            #target.send('\n')
            
       
# Aggregate LSH results to generate a candidate set
@coroutine
def reducelsh(target):
    oldkey = ''
    values = set([])
    while True:
        line = (yield)

        if len(line.rstrip()):

            (key, value) = line.rstrip().split('\t')
                
            if key == oldkey:
                values.add(value)
            else:
                if len(values) > 2:
                    joinedValues = '\t'.join(values)
                    target.send( '%s\t%s\n' % (joinedValues,oldkey) )
                values = set([value])
                oldkey = key
