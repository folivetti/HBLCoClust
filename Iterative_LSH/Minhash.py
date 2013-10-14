import numpy as np
import matplotlib.pyplot as plt
from collections import Counter
from scipy.stats import chisquare
from random import sample
import gc

def test_uniformity(hist, bins):
    c = np.zeros(bins)
    for h in hist:
        c[h] += 1
    pvalue = chisquare(c)[1]
    likelihood = ('possible' if pvalue > 0.05 else 'unlikely')
                  #'unlikely' if pvalue > 0.01 else 'HIGHLY UNLIKELY')

    return pvalue, likelihood
    
n_hashes = 50000 # 5x to 10x
n_bins = 5000
P = 7577
n_feats = 5000
no_of_experiments = 100

c_rnd = Counter()
c_seq = Counter()
c_rnd_min = Counter()
c_seq_min = Counter()

#print 'Experiment\t\tp-value\tResult'

for it in range(no_of_experiments):
    x = np.random.random_integers(1,P-1)

    a = np.random.random_integers(1,P-1,n_hashes)
    b = np.random.random_integers(0,P-1,n_hashes)

    hf = []
    for aa,bb in zip(a,b):
        l = lambda xx, a0=aa, b0=bb: np.mod(a0*xx + b0,P)
        hf.append(l)
    hist = np.array([np.mod(h(x), n_bins) for h in hf])
    (pvalue, result) = test_uniformity(hist,n_bins)
    c_rnd.update([result])
    #print 'Random hashes:\t\t%.2f\t%s' % (pvalue, result)
    del hist
    gc.collect()
    
    k1, k2 = a[0]*x, b[0]+x
    k = np.mod(k1+k2,P)
    hk = np.zeros(n_hashes)
    ik=k1
    for i in range(n_hashes):
        ik = np.mod(ik+k,P)
        hk[i] = ik#np.mod(ik,P)
        #ik += k
    hk = np.mod(hk,n_bins)
    pvalue, result = test_uniformity(hk,n_bins)
    c_seq.update([result])
    #print 'Seq. hashes:\t\t%.2f\t%s' % (pvalue, result)
    del hk
    gc.collect()
    
    x = np.array(sorted(sample(range(1,P-1),n_feats)))

    hist = np.array([np.argmin(h(x)) for h in hf])
    pvalue, result = test_uniformity(hist,n_feats)
    c_rnd_min.update([result])
    #print 'Random min-hashes:\t%.2f\t%s' % (pvalue, result)
    del hist
    gc.collect()    

    hk = np.zeros((n_feats, n_hashes))
    z1 = a[0]*x
    z2 = b[0]+x #np.remainder(b[0]+x,P)
    for i in range(n_hashes):
        z1 = np.remainder(z1+z2,P)        
        hk[:,i]= z1#np.remainder(z1,P)
    hf = np.argmin(hk,axis=0)
    pvalue, result = test_uniformity(hf,n_feats)
    c_seq_min.update([result])
    #print 'Seq. min-hashes:\t%.2f\t%s' % (pvalue, result)
    #print '----------------'
    del hk
    del hf
    gc.collect()

print 'Final results: '
print 'Random hashes:\t%s' % c_rnd
print 'Seq. hashes:\t%s' % c_seq
print 'Random min-hashes:\t%s' % c_rnd_min
print 'Seq. min-hashes:\t%s' % c_seq_min
print '---------------'
