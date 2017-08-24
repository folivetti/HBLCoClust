import json
from collections import defaultdict
k = open('biclusters/u1baseH75.bic','r')
m = open('biclusters/u1baseH75Neg.bic','r')
h = open('u1test.txt','r')
dbicpos = {}
dbicneg = {}
dprednotas = {}

bicpos = []
whichupos = defaultdict(list)
for it,linebic in enumerate(k):
    bic = json.loads(linebic)
    bicpos.append(bic['feats'])
    for u in bic['objs']:
        whichupos[u].append(it)
k.close()

bicneg = []
whichuneg = defaultdict(list)
for it,linebic in enumerate(m):
    bic = json.loads(linebic)
    bicneg.append(bic['feats'])
    for u in bic['objs']:
        whichuneg[u].append(it)
m.close()

for l in h:
    positivo_positivo, negativo_negativo = 0, 0
    u, mov, r = map(str, l.split('\t'))
    movpos = mov.replace(' ','')+'#Y'
    movneg = mov.replace(' ','')+'#N'
    bicposlist = whichupos[u]
    bicneglist = whichuneg[u]
    
    for idx in bicposlist:
        if movpos in bicpos[idx]:
            positivo_positivo += 1
    for idx in bicneglist:
        if movneg in bicneg[idx]:
            negativo_negativo += 1    
    if (positivo_positivo != 0 or negativo_negativo != 0):
        if positivo_positivo > negativo_negativo:
            dprednotas[(u, mov, r.strip())] = 1
        elif positivo_positivo < negativo_negativo:
            dprednotas[(u, mov, r.strip())] = 1
   

cont_err, cont_ind = 0, 0
msre, mae = 0.0, 0.0

for tripla in dprednotas.keys():
    if int(tripla[2]) >= 3:
        rating = 1
    else:
        rating = 0
    msre += (rating-dprednotas[tripla])**2
    mae += abs(rating-dprednotas[tripla])
    if rating == dprednotas[tripla]:
        cont_ind += 1
    cont_err += 1
    
print 'MSRE =', msre/cont_err
print 'MAE =', mae/cont_err
print 'Acertos:', cont_ind, 'de um total de', cont_err
    























