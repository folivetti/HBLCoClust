import json
from collections import defaultdict
h = open('u1test.txt','r')


positivo_positivo, positivo_negativo, negativo_positivo, negativo_negativo = 0, 0, 0, 0

bicpos = []
whichupos = defaultdict(list)
k = open('biclusters/u1base.bic','r')
for it,linebic in enumerate(k):
    bic = json.loads(linebic)
    bicpos.append(bic['feats'])
    for u in bic['objs']:
        whichupos[u].append(it)
k.close()

bicneg = []
whichuneg = defaultdict(list)
m = open('biclusters/u1baseNeg.bic','r')
for it,linebic in enumerate(m):
    bic = json.loads(linebic)
    bicneg.append(bic['feats'])
    for u in bic['objs']:
        whichuneg[u].append(it)
m.close()

count1=0
count2=0
for l in h:
    u, mov, r = map(str, l.split('\t'))
    movpos = mov.replace(' ','')+'#Y'
    movneg = mov.replace(' ','')+'#N'
    bicposlist = whichupos[u]
    bicneglist = whichuneg[u]
    count1+=1
    hascounted = False
    
    if int(r) >= 3:
        for idx in bicposlist:
            if movpos in bicpos[idx]:
                positivo_positivo += 1
                if not hascounted:
                    hascounted = True
                    count2+=1
                break
        for idx in bicneglist:
            if movneg in bicneg[idx]:
                positivo_negativo += 1
                if not hascounted:
                    hascounted = True
                    count2+=1
                break
    elif int(r) < 3:
        for idx in bicposlist:
            if movpos in bicpos[idx]:
                negativo_positivo += 1
                if not hascounted:
                    hascounted = True
                    count2+=1
                break
        for idx in bicneglist:
            if movneg in bicneg[idx]:
                negativo_negativo += 1
                if not hascounted:
                    hascounted = True
                    count2+=1
                break

print positivo_positivo, positivo_negativo, negativo_positivo, negativo_negativo
print count1, count2
