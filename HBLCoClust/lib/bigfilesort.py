## {{{ http://code.activestate.com/recipes/576755/ (r3)
# based on Recipe 466302: Sorting big files the Python 2.4 way
# by Nicolas Lehuen

import os
from tempfile import gettempdir
from itertools import islice, cycle
from collections import namedtuple
import heapq

Keyed = namedtuple("Keyed", ["key", "obj"])

def merge(key=None, *iterables):
    # based on code posted by Scott David Daniels in c.l.p.
    # http://groups.google.com/group/comp.lang.python/msg/484f01f1ea3c832d

    if key is None:
        for element in heapq.merge(*iterables):
            yield element
    else:
        keyed_iterables = [(Keyed(key(obj), obj) for obj in iterable)
                        for iterable in iterables]
        for element in heapq.merge(*keyed_iterables):
            yield element.obj
            
def batch_sort(input, output, key=None, buffer_size=32000, tempdirs=None):
    if tempdirs is None:
        tempdirs = []
    if not tempdirs:
        tempdirs.append(gettempdir())

    chunks = []
    csize = 1024*1024*1024
    try:
        with open(input,'rb',csize) as input_file:
            input_iterator = iter(input_file)
            for tempdir in cycle(tempdirs):
                current_chunk = list(islice(input_iterator,buffer_size))
                if not current_chunk:
                    break
                current_chunk.sort(key=key)
                output_chunk = open(os.path.join(tempdir,'%06i'%len(chunks)),'w+b',csize)
                chunks.append(output_chunk)
                output_chunk.writelines(current_chunk)
                output_chunk.flush()
                output_chunk.seek(0)
        with open(output,'wb',csize) as output_file:
            output_file.writelines(merge(key, *chunks))
    finally:
        for chunk in chunks:
            try:
                chunk.close()
                os.remove(chunk.name)
            except Exception:
                pass
