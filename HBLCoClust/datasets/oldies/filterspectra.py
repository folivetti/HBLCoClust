f = open('spectraChart26.data')
fw = open('spectraChart26_3.data',  'w')
for l in f:
    if l.split('\t')[1][0]=='3':
        fw.write(l)
f.close()
fw.close()
