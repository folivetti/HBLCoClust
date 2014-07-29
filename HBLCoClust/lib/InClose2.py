import numpy as np
import json

def InClose2( dataname, (drows, dcols), min_rows, min_cols ):
    # In-Close2:        

    fw = open('biclusters/'+dataname+'InClose.bic','w')
    # create binary data matrix D    
    lrows = drows.keys()
    lcols = dcols.keys()
    D = np.zeros( (len(lrows),len(lcols)) )
    for i,o in enumerate(lrows):
        D[i,[j for j,f in enumerate(lcols) if f in drows[o]]] = 1

    Stack = [( set(range(D.shape[0])), set(),0 )] # stack of candidates Ar, Br and y
    while len(Stack):
        (A,B,y) = Stack.pop(0)
        J = []
        An = []
        for j in range(y,D.shape[1]):
            if j not in B:
                RW = A & set(D[:,j].nonzero()[0])
                if len(RW) == len(A):
                    B.add(j)
                elif len(RW)>=min_rows and all(D[list(RW),k].sum()<len(RW) for k in set(range(j)) - B):  # is RW canonical?
                    J.append(j)
                    An.append(RW)
        # dump bicluster found           
        if len(A) >= min_rows and len(B) >= min_cols:
            hardrow = [lrows[i] for i in A]                
            hardcol = [lcols[i] for i in B]

            bicluster = {"objs":hardrow,"feats":hardcol}
            fw.write( json.dumps(bicluster) + '\n' )

        # stack all remaining candidates
        for y,A in zip(J,An):
            Bn = B | set([y])
            Stack.append((A,Bn,y))
