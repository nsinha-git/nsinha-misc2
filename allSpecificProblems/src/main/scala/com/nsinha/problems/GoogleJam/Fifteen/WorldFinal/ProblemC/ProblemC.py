"""
Author: Nish Sinha
"""

def find_all_substrings(s: str):
    res = []
    for i in range(0, len(s)):
        res.extend(find_all_substrings_with_start(s, i))

    return res


def find_all_substrings_with_start(s: str, start):
    res = []
    start
    rem = len(s) - start
    for i in range(1, rem + 1):
        res.append(s[start: start + i])
    return res

def closest_match(f: float, closest_yet_f: float, potential: str, eval_cand: str):
    cur_prop = len([s for s in eval_cand if s == '1'])/float(len(eval_cand))

    if (abs(f -cur_prop) < abs(f - closest_yet_f)):
        return (cur_prop, eval_cand)
    else:
        return (closest_yet_f, potential)


def solve(s: str, f: float):
    all_substr = find_all_substrings(s)
    all_substr_set = set(all_substr)
    potential = s
    closest_yet = f + 1000
    potential_acc = set()

    for sub in all_substr_set:
        (closest_yet_new, potential_new) = closest_match(f, closest_yet, potential, sub)
        if (closest_yet_new != closest_yet):
            potential_acc = set()

        potential_acc.add(potential_new)
        potential = potential_new
        closest_yet = closest_yet_new



    print(s.index(potential), potential, closest_yet)





if __name__ == "__main__":
    s = "001001010111"
    f = 0.66667
    solve(s, f)


