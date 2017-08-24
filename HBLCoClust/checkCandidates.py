dataset = 'multi5'

objs = {}
f = open('datasets/'+dataset+'.agg')
for l in f:
    (oid,feats) = l.split('\t')
    if oid in objs:
        print oid
    objs[oid]=1
f.close()

objscand = {}
f = open('candidates/'+dataset+'.cand')
for l in f:
    oids = l.split('\t')
    oids, feats = oids[:-1],oids[-1]
    feats = set(feats.rstrip().split('_'))
    if len(feats)>2 and len(oids)>1:
        for o in oids:
            objscand[o]=1
f.close()

print len(objs.keys()), len(objscand.keys()), float(len(objscand.keys()))/len(objs.keys())
