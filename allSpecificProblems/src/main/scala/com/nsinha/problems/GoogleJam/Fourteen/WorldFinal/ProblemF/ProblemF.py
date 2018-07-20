"""
Author: Nish Sinha
"""
import sortedcontainers
from math import ceil



def gen_all_offsets():
    global offsets

    for i in range(0, R+1):
        for j in range(0, G*R+1):
            offset = i*G + j;
            if (offset <= R*G):
                offsets.add(offset)

def create_rewards(N):
    global rewards

    p = float(sum(N)/float(len(N)))
    last = p
    rewards[0] = p
    for i in range(1, R + 1):
        acc = 0.0
        for j in range(0, len(N)):
            if (N[j] < p):
               acc +=  last/len(N)
            else:
                acc += N[j]/float(len(N))

        rewards[i] = acc
        last = rewards[i]



def create_a_map_offset_p():
    mp = sortedcontainers.SortedDict()
    global offsets

    for i in  offsets:
        mp[i] = 0
    return mp

def compare_two_map_not_converged(mp1: sortedcontainers.SortedDict, mp2: sortedcontainers.SortedDict):
    for i in mp1.keys():
        if ((abs(mp1[i] - mp2[i])/(mp1[i] + 1e-8)) > 1e-6):
            return True

    return False

def copy_map (mp1, mp2):
    for i in mp1.keys():
        mp2[i]=mp1[i]


def play(cur_prob, repeats, total):
    return (cur_prob * total + rewards[repeats])/(total + 1)





def play_more(mp2, last_total):
    mp3 = create_a_map_offset_p()
    for i in mp2.keys():
        remaining_tokens = i
        cur_prob = mp2[i]
        reroll = 0
        next_prob = play(cur_prob, reroll, last_total)
        index = remaining_tokens + 1
        if (index > R*G): index = R*G
        if (mp3[index] < next_prob):
            mp3[index] = next_prob
        while (remaining_tokens >= G):
            remaining_tokens -= G;
            reroll += 1
            next_prob = play(cur_prob, reroll, last_total)
            index = remaining_tokens + 1
            if (index > R*G): index = R*G
            if (mp3[index] < next_prob):
                mp3[index] = next_prob

    mp2 = mp3
    return mp2






if __name__ == '__main__':
    N = [1,0,0.5]
    R = 1
    G = 1
    offsets = sortedcontainers.SortedSet();
    rewards = sortedcontainers.SortedDict();
    gen_all_offsets();
    mp1 = create_a_map_offset_p()
    mp2 = create_a_map_offset_p()
    copy_map(mp1,mp2)
    create_rewards(N)
    total = 0
    while True:
        mp2 = play_more(mp2, total)
        total += 1
        if compare_two_map_not_converged(mp1, mp2) == True:
            copy_map(mp2,mp1)
            continue
        else:
            break;
    print(max(mp1.values()))





