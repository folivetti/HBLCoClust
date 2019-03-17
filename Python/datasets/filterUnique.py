from collections import defaultdict

freq = defaultdict(int)

f = open('artur.data')
for l in f:
  fid, w = l.strip().split('\t')
  freq[w] += 1
f.close()

f = open('artur.data')
fw = open('artur2.data', 'w')
for l in f:
  fid, w = l.strip().split('\t')
  if freq[w] > 1:
      fw.write(l)
f.close()
fw.close()
