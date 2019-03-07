# IZPIT 24.1.2019
# PYTHON
# 3. NALOGA

from functools import lru_cache

def najhitrejsi_cas(seznam):
    @lru_cache(maxsize=None)
    def pomozna(indeks, energija):
        moznosti = []
        if indeks >= len(seznam):
            return 0
        else:
            for skok in range(1, (energija + 1)):
                if (indeks + skok) >= len(seznam):
                    return 1
                else:
                    moznost = 1 +  pomozna(indeks + skok, energija - skok + seznam[(indeks + skok)])
                    moznosti.append(moznost)
            return min(moznosti)
    return pomozna(0, seznam[0])

test1 = [2,4,1,2,1,3,1,1,5]
test2 = [4,1,8,2,11,1,1,1,1,1]
test3 = [10] * 50

print(najhitrejsi_cas(test1))
print(najhitrejsi_cas(test2))
print(najhitrejsi_cas(test3))

### =) =) =) ###
    

        


