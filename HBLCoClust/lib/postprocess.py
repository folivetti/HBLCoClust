import json
from scipy.cluster.hierarchy import single, linkage, complete, average, ward
from collections import defaultdict

def Jaccard(b1,b2):
    sb1 = set(b1['objs'])
    sb2 = set(b2['objs'])
    inter = sb1 & sb2
    union = sb1 | sb2
    return 1.0 - len(inter)/float(len(union))

def hierclust(dataset,k):
    f = open('biclusters/'+dataset+'M.bic')
    bics = []
    for l in f:
        bics.append( json.loads(l) )
    f.close()

    dist = []
    for i,b1 in enumerate(bics):
        for b2 in bics[i+1:]:
            dist.append(Jaccard(b1,b2))

    clusters = average(dist)    
    clustdict = {i:[i] for i in xrange(len(clusters)+1)}
    for i in xrange(len(clusters)-k+1):
        clust1= int(clusters[i][0])
        clust2= int(clusters[i][1])
        clustdict[max(clustdict)+1] = clustdict[clust1] + clustdict[clust2]
        del clustdict[clust1], clustdict[clust2]

    newbics = []
    for clusts in clustdict.values():        
        objs = reduce(lambda x,y: x+y,[bics[idx]['objs'] for idx in clusts])
        feats = reduce(lambda x,y: x+y,[bics[idx]['feats'] for idx in clusts])
        newbics.append({'objs': list(set(objs)), 'feats':list(set(feats))})
        
    fw = open('biclusters/'+dataset+'.bic','w')
    for bic in newbics:
        fw.write(json.dumps(bic)+'\n')
    fw.close()
    
def merge(dataset, inclose=False):
    if inclose==True:
        f1 = open('biclusters/'+dataset+'InClose.bic')
        f2 = open('biclusters/'+dataset+'InClose.bic')
    else:
        f1 = open('biclusters/'+dataset+'M.bic')
        f2 = open('biclusters/'+dataset+'M.bic')
    fw = open('biclusters/'+dataset+'.bic','w')
    bic = {'_id':0}
    count=0
    tabu = {}

##    for l in f1:
##        bic = json.loads(l)
##        fw.write(json.dumps(bic)+'\n')

    bic = defaultdict(list)
    for l in f1:
        b = json.loads(l)
        bic[';'.join(b['feats'])] += b['objs']
    for k,v in bic.iteritems():
        b['feats']=k.split(';')
        b['objs']=list(set(v))
        fw.write(json.dumps(b)+'\n')

    '''
    for i,l1 in enumerate(f1):
        count+=1
        if i in tabu:
            continue
        tabu[i]=1
        bic1 = json.loads(l1)
        sb1 = set(bic1['feats'])
        for j,l2 in enumerate(f2):
            if j in tabu:
                continue
            bic2 = json.loads(l2)
            #inter = set(bic1['feats']) & set(bic2['feats'])
            #union = set(bic1['feats']) | set(bic2['feats'])
            #J = len(inter)/float(len(union))
            sb2 = set(bic2['feats'])
            if sb1 == sb2:
            #if J >= 1.0:
                union = sb1 | sb2
                bic1['feats']=list(union)
                bic1['objs']=list(set(bic1['objs']) | set(bic2['objs']))
                tabu[j]=1
        #if len(bic1['objs'])>2 and len(bic1['feats'])>5:
        fw.write(json.dumps(bic1)+'\n')
        f2.seek(0)
    '''
    fw.close()
    f1.close()
    f2.close()

    print 'it was: ',count

def uncovered(dataset, (oi,fi)):
    f = open('biclusters/'+dataset+'.bic')
    covered = set([])
    bics=[]
    for l in f:
        bic = json.loads(l)
        covered = covered | set(bic['objs'])
        bics.append(bic)
    uncovered = set(oi.keys()) - covered
    f.close()

    print 'uncovered: ', len(uncovered)
    fw = open('uncuvered.txt','w')
    fw.write(' '.join(uncovered))
    fw.close()
    for o in uncovered:
        maxbic = 0
        maxJ = 0
        for i,bic in enumerate(bics):
            inter = set(bic['feats']) & set(oi[o])
            union = set(bic['feats']) | set(oi[o])
            J = len(inter)/float(len(union))
            if J > maxJ:
                maxJ=J
                maxbic=i
        bics[maxbic]['objs'].append(o)
    fw = open('biclusters/'+dataset+'.bic','w')
    for bic in bics:
        fw.write(json.dumps(bic)+'\n')
    fw.close()

def mergecand( candata ):
    f = open(candata)
    fw = open('tmp.txt','w')
    currobj=set([])
    currfeat=''
    for l in f:
        objs = l.split('\t')
        if len(currobj)==0:
            currobj = set(objs[:-1])
            currfeat = objs[-1]
        else:
            newobj = set(objs[:-1])
            newfeat = objs[-1]
            if currobj <= newobj:
                currobj |= newobj
            elif newobj <= currobj:
                currobj |= newobj
                currfeat = newfeat
            else:
                fw.write('\t'.join(list(currobj))+'\t'+currfeat)
                currobj = newobj
                currfeat = newfeat
    fw.write('\t'.join(list(currobj))+'\t'+currfeat)
    fw.close()
    f.close()

    f = open('tmp.txt')
    fw = open(candata,'w')
    for l in f:
        fw.write(l)
    f.close()
    fw.close()

def filterbics(bicname):        
    f = open(bicname)
    bics = []
    for l in f:
        bics.append( json.loads(l) )
    f.close()

    sbics = sorted(bics, key=lambda x: len(x['objs'])*len(x['feats']), reverse=False)

    newbic=[]
    covered = {}
    for bic in sbics:
        if not all( o in covered for o in bic['objs'] ) or not all( f in covered for f in bic['feats'] ):
            newbic.append(bic)
            for o in bic['objs']:
                covered[o] = 1
            for f in bic['feats']:
                covered[f] = 1

    fw=open(bicname,'w')
    for bic in newbic:
        fw.write(json.dumps(bic)+'\n')
    fw.close()
