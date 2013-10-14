import numpy as np
from scipy import stats

dataset=[]
f = open('multi5.agg')
for l in f:
    oid, feats = l.split('\t')
    feats = set(feats.rstrip().split())
    dataset.append(feats)
f.close()
dataset = np.array(dataset)
nobj = dataset.shape[0]

# Calculate real Jaccard values
J = np.zeros( (nobj,nobj) )
for i,f1 in enumerate(dataset):
    for j,f2 in enumerate(dataset):
        J[i,j] = len(f1 & f2)/float(len(f1 | f2))


# number of hashes to test
n_hashes = 20
P = 7577

# gen random hashes (not optimized)
a = np.random.random_integers(1,P-1,n_hashes)
b = np.random.random_integers(0,P-1,n_hashes)
hf = []
randMinHash = np.zeros((nobj,n_hashes))
for aa,bb in zip(a,b):
    l = lambda xx, a0=aa, b0=bb: np.argmin(np.mod(a0*xx + b0,P))
    hf.append(l)
for i in range(nobj):
    x = np.array(map(hash,dataset[i]))
    randMinHash[i,:] = np.array([h(x) for h in hf])

    
# gen iter. hashes (not optimized)
iterMinHash = np.zeros((nobj, n_hashes))
for i in range(nobj):
    x = np.array(map(hash,dataset[i]))
    hk = np.zeros( (x.shape[0],n_hashes) )
    
    z1 = a[0]*x
    z2 = b[0]+x 
    for j in range(n_hashes):
        z1 = np.remainder(z1+z2,P)        
        hk[:,j]= z1#np.remainder(z1,P)
    hf = np.argmin(hk,axis=0)
    iterMinHash[i,:] = hf

# calculate error of estimation
rmse1=[]
rmse2=[]
mae1=[]
mae2=[]
for i in range(nobj):
    for j in range(nobj):
        if i<j:
            eJ1 = (n_hashes - np.count_nonzero(randMinHash[i,:]-randMinHash[j,:])) / float(n_hashes)
            eJ2 = (n_hashes - np.count_nonzero(iterMinHash[i,:]-iterMinHash[j,:])) / float(n_hashes)

            mae1.append( abs(eJ1-J[i,j]) )
            mae2.append( abs(eJ2-J[i,j]) )

            rmse1.append( (eJ1-J[i,j])**2 )
            rmse2.append( (eJ2-J[i,j])**2 )
            
mae1 = np.array(mae1)
mae2 = np.array(mae2)
rmse1 = np.array(rmse1)
rmse2 = np.array(rmse2)

print '# of hashes:', n_hashes
print '# of objects:', nobj
print 'Avg. Jaccard: ', np.mean(J)
print '\tRandom\t\tIter'
print 'MAE: \t',np.mean(mae1),np.std(mae1),'\t', np.mean(mae2),np.std(mae2)
print 'RMSE: \t',np.sqrt(np.mean(rmse1)),'\t', np.sqrt(np.mean(rmse2))
print stats.ttest_rel(mae1,mae2)
print stats.ttest_rel(rmse1,rmse2)
