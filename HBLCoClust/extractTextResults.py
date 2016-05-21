from itertools import cycle
import numpy as np

datasets = ['classic3', 'multi5', 'multi10']
algs = ["hbl"]
metrics = ["nbics","nobjs","nfeats","rows","cols","vol","prec","nmi","pmi"]

results = {}
for d in datasets:
  results[d] = {}
  for a in algs:
    results[d][a] = {}
    for m in metrics:
      results[d][a][m] = []

f = open("textResults.txt")
lines = f.readlines()
f.close()

idx = 0

for dataset in cycle(datasets):
  a = algs[0]
  if "From a total" not in lines[idx+6]:
    print lines[idx+6]
  results[dataset][a]["nbics"].append(int(lines[idx+6].split()[-2]))
  results[dataset][a]["nobjs"].append(float(lines[idx+7].split("%")[0]))
  results[dataset][a]["nfeats"].append(float(lines[idx+8].split("%")[0]))
  results[dataset][a]["rows"].append(float(lines[idx+9].split("=")[1].split("+")[0]))
  results[dataset][a]["cols"].append(float(lines[idx+10].split("=")[1].split("+")[0]))
  results[dataset][a]["vol"].append(float(lines[idx+11].split("=")[1]))
  results[dataset][a]["prec"].append(float(lines[idx+13]))
  results[dataset][a]["nmi"].append(float(lines[idx+14]))
  results[dataset][a]["pmi"].append(float(lines[idx+15]))
    
  idx += 16
  if idx >= 1440: #len(lines):
    break

for d in datasets:
  for a in algs:
    for m in metrics:
      results[d][a][m] = np.array(results[d][a][m]) 

for d in datasets:
  print d
  print "\t", "\t".join(algs)
  for m in metrics:
    print m,"\t", 
    print "%.2f" % (results[d][algs[0]][m].mean())
  print
