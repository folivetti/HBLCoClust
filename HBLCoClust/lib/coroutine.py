def coroutine(func):
    def start(*args,**kwargs):
        cr = func(*args,**kwargs)
        cr.next()
        return cr
    return start

def source(filename, target):
    for l in open(filename):
        target.send(l)

@coroutine
def sink(filename):
    with open(filename,'w') as fw:
        while True:
            line = (yield)
            fw.writelines(line)
    f.closed

@coroutine
def sinkApp(filename):
    with open(filename,'a') as fw:
        while True:
            line = (yield)
            fw.writelines(line)
    f.closed    
