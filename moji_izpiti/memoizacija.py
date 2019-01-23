# PYTHON 

def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f
    
# @memoiziraj 


from functools import lru_cache

# @lru_cache(maxsize=None)
