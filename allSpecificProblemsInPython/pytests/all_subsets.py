


nos = [1,2 ,3]
subsets = []


def gen_subs():
    for i in range(0,len(nos) + 1):
        subsets.extend(choose(i, nos))

    for i in subsets:
        print(i)

def choose(how_many, all_nos): # -> []
    if (how_many == 0):
        return [[]]
    new_subsets = []
    for i in range(0,len(all_nos)):
        choose_one = all_nos[i]
        new_all_nos = all_nos[i+1:len(all_nos)]
        for subset in choose(how_many -1, new_all_nos):
            subset.append(choose_one)
            new_subsets.append(subset)
    return new_subsets

def gen_all_combs(how_many):
    a = choose(how_many, nos)
    for i in a:
        print(i)

def gen_all_perms(all_nos):
    res = []
    for i in range(0,min(1, len(all_nos))):
        choose_one = all_nos[i]
        remains = all_nos.copy()
        remains.remove(choose_one)
        remains_perms = gen_all_perms(remains)
        for r in remains_perms:
            for j in range(0, len(r) + 1):
                m = r.copy()
                m.insert(j, choose_one)
                res.append(m)
    if len(res) != 0:
        return res
    else:
        return [[]]

print(len(gen_all_perms([1,2,3,4,5])))
