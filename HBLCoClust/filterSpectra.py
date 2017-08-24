import json

labels = {}
f2 = open('/home/olivetti/Downloads/spectraChart26.data.map')
for l in f2:
    fields = l.strip().split(':')
    labels[fields[0]] = ':'.join(fields[1:])
f2.close()

score = {}
f1 = open('/home/olivetti/Downloads/vol/tmp/heidensi/d4j4/Chart/26/.BugLoRD_data/ranking/tarantula/ranking.rnk')
for l in f1:
    fields = l.split(':')
    id = ':'.join(fields[:-1])
    tarantula = float(fields[-1])
    score[id] = tarantula
f1.close()

f = open('biclusters/spectraChart26_3InClose.bic')
fw = open('biclusters/spectraChart26fail.bic', 'w')
i=0
for l in f:
    bic = json.loads(l)
    if any('3.' in f for f in bic['feats']):
        t = 0.0
        print i, len(bic['objs'])
        for o in bic['objs']:
            t += score[labels[o]]
            print labels[o],':',score[labels[o]]
        bic['score'] = t/float(len(bic['objs']))
        print ''
        i = i+1
        #print ','.join(bic['objs']),':',bic['score']
        print('Avg: ', bic['score'])

        fw.write(json.dumps(bic))
        fw.write('\n')
f.close()
fw.close()
