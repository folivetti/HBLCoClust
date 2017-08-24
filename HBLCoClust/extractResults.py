from itertools import cycle
import numpy as np

datasets = ['House Votes 84','Zoo','Soybean Large','Soybean Small']
algs = ["hbl", "colsh","inclose"]
metrics = ["nbics","nobjs","nfeats","rows","cols","vol","prec","nmi","pmi"]

results = {}
for d in datasets:
  results[d] = {}
  for a in algs:
    results[d][a] = {}
    for m in metrics:
      results[d][a][m] = []

f = open("catResults.txt")
lines = f.readlines()
f.close()
f = open("catInCloseresults.txt")
lines2 = f.readlines()
f.close()

idx = 0

for dataset in cycle(datasets):
  if lines[idx].strip() == dataset: # ok
    a = algs[0]
    results[dataset][a]["nbics"].append(int(lines[idx+10].split()[-2]))
    results[dataset][a]["nobjs"].append(float(lines[idx+11].split("%")[0]))
    results[dataset][a]["nfeats"].append(float(lines[idx+12].split("%")[0]))
    results[dataset][a]["rows"].append(float(lines[idx+13].split("=")[1].split("+")[0]))
    results[dataset][a]["cols"].append(float(lines[idx+14].split("=")[1].split("+")[0]))
    results[dataset][a]["vol"].append(float(lines[idx+15].split("=")[1]))
    results[dataset][a]["prec"].append(float(lines[idx+17]))
    results[dataset][a]["nmi"].append(float(lines[idx+18]))
    results[dataset][a]["pmi"].append(float(lines[idx+19]))
    
    a = algs[1]
    results[dataset][a]["nbics"].append(int(lines[idx+21].split()[-2]))
    results[dataset][a]["nobjs"].append(float(lines[idx+22].split("%")[0]))
    results[dataset][a]["nfeats"].append(float(lines[idx+23].split("%")[0]))
    results[dataset][a]["rows"].append(float(lines[idx+24].split("=")[1].split("+")[0]))
    results[dataset][a]["cols"].append(float(lines[idx+25].split("=")[1].split("+")[0]))
    results[dataset][a]["vol"].append(float(lines[idx+26].split("=")[1]))
    results[dataset][a]["prec"].append(float(lines[idx+28]))
    results[dataset][a]["nmi"].append(float(lines[idx+29]))
    results[dataset][a]["pmi"].append(float(lines[idx+30]))

    if idx < 100:
      a = algs[2]
      results[dataset][a]["nbics"].append(int(lines2[idx+21].split()[-2]))
      results[dataset][a]["nobjs"].append(float(lines2[idx+22].split("%")[0]))
      results[dataset][a]["nfeats"].append(float(lines2[idx+23].split("%")[0]))
      results[dataset][a]["rows"].append(float(lines2[idx+24].split("=")[1].split("+")[0]))
      results[dataset][a]["cols"].append(float(lines2[idx+25].split("=")[1].split("+")[0]))
      results[dataset][a]["vol"].append(float(lines2[idx+26].split("=")[1]))
      results[dataset][a]["prec"].append(float(lines2[idx+28]))
      results[dataset][a]["nmi"].append(float(lines2[idx+29]))
      results[dataset][a]["pmi"].append(float(lines2[idx+30]))

    idx += 32
  if dataset == datasets[-1]:
    idx -= 1
  if idx >= len(lines):
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
    print "%.2f\t%.2f\t%.2f" % (results[d][algs[0]][m].mean(), results[d][algs[1]][m].mean(), results[d][algs[2]][m].mean())
  print
