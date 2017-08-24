import time
from lib.coroutine import coroutine, source, sink, sinkApp
import os
from bigfilesort import batch_sort

def sortfile(s,t):
    stat = os.stat(s)
    if stat.st_size < 524288000:
        target = sink(t)
        for l in sorted(open(s).readlines()):
            target.send(l)
    else:
        target = sink(t)
        # create temp dir
        try:
            os.stat('temp')
        except:
            os.mkdir('temp')
            os.mkdir('temp/lsh')
        f = open(s)
        fw = {}
        for c in 'abcdefghijklmnopqrstuvwxyz0':
            fw[c] = open('temp/'+s+'_'+c,'w')
        for l in f:
            if l[0].lower() in fw:
                fw[l[0].lower()].write(l)
            else:
                fw['0'].write(l)
        for c in 'abcdefghijklmnopqrstuvwxyz0':
            fw[c].close()
            for l in sorted(open('temp/'+s+'_'+c).readlines()):
                target.send(l)
            os.remove('temp/'+s+'_'+c)
        os.rmdir('temp/lsh')
        os.rmdir('temp')
        
            

@coroutine
def gettime():
    st = time.time()
    while True:
        name = (yield)
        st2 = time.time()
        print '%s time = %f' % (name,st2-st)
        st = st2

def link(src,function,target,params=None,append=False):
    if append:
        if params:
            source(src,function(sinkApp(target),params))
        else:
            source(src,function(sinkApp(target)))
    else:
        if params:
            source(src,function(sink(target),params))
        else:
            source(src,function(sink(target)))
