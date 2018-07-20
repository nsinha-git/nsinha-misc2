import  functools
from collections import deque

levels = [(0,5), (0,1), (1,1), (4,7), (5,6)]

def comp_levels(l, m):
    if (l[1]> m[1]):
        return 1
    else:
        if (l[1] < m[1]):
            return -1
        else:
            if (l[0] > m[0]):
                return 1
            else:
                if (l[0] < m[1]):
                    return -1
    return 0


sorted_levels = sorted(levels,key= functools.cmp_to_key(comp_levels),reverse=True)

attempts = 0

deq = deque(sorted_levels)
stars = 0
while len(deq) > 0:
    cur_el = deq.pop()
    attempts +=1
    if cur_el[1] <= stars:
        stars += 1
        if (cur_el[0] >= 0):
            stars += 1
        continue
    else:
        if cur_el[0] >= 0 and cur_el[0] <= stars:
            cur_el = (-1,cur_el[1])
            stars += 1
            deq.append(cur_el)
        else:
            attempts = -1
            print("impossoble")
            break;

if (attempts > 0):
    print(attempts)



