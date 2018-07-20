import sortedcontainers

mem = sortedcontainers.SortedDict()
min_value_found = 0;


def solve(n: int, A :int, B: int, r: int):#returns days
    global  min_value_found, breakOut
    #generate all m*A + n*B
    res = max_for_days(n, A, B, r) - n
    if (res < 0):
        j=solve(n, A, B, r + A)
        k=solve(n, A, B, r + B)
        return min(j,k)
    else:
        if (min_value_found > r):
            min_value_found = r
            breakOut = True
            solve2(n,A,B)
        return r

def solve2(n,A,B):
    index = mem.bisect_left(min_value_found)
    if (index > 1):
        low = mem.keys()[index-1]
        high = min_value_found
        print(linear_search(low, high))
    else:
        print(index)


def linear_search(low, hi):
    i = low + 1
    while(True):
        if (max_for_days(n,A, B, i) > n):
            print(i)
            raise Exception
        else: i += 1

def max_for_days(n: int, A, B, cur_days: int) -> int: #returns max for cur_days
    global mem
    if (min_value_found != 0 and cur_days > min_value_found):
        return mem[min_value_found]
    if (mem.get(cur_days) is None):
        if (cur_days < A): return mem[A]
        if (cur_days < B): return mem[A]
        mem[cur_days] = max_for_days(n, A, B, cur_days - A) + max_for_days(n, A, B, cur_days -B)

    return mem[cur_days]

if __name__ == '__main__':
    n= 574503743127099
    A= 55
    B= 66
    mem[B] = 3
    mem[A] = 2
    min_value_found = n *B;

    breakOut = False
    print(solve(n, A, B, 0))
    solve(n, A, B, 0)





