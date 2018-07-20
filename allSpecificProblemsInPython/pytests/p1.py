import pytest
from sortedcollections import SortedList,SortedDict, SortedSet

A = 2
B = 5
Pr = [0.6,0.6, 0.6, 0.6,0.6]
G  = []
PrG = []
Strats = []



def gen_all_group():
    '''
    generates G
    :return:
    '''
    for i in range(0, A+1):
        G.append(i)

    print(G)


def gen_pr():
    '''
    generates PrG
    :return:
    '''
    for g in G:
        p = 1.0
        for i in range(0,g):
            p = p * Pr[i]

        if (g < A):
            #if g >= A means that all A's are correct
            p = p* (1 - Pr[g])
        PrG.append(p)

    print(PrG)


def gen_all_strats():

    for i in range(0, A+1):
        Strats.append(i)

    print(Strats)

def eval_all_strats():
    min_steps = 100*B

    for s in Strats:
        steps = 0
        for g in G:
            steps += eval(s, g, PrG[g])

        min_steps = min(min_steps, steps)

    min_steps = min(min_steps, B+2)
    print(min_steps)

def eval(s,g,pr):
    nbs = s
    err_loc = g
    steps = 0
    if (err_loc < A - nbs):
        steps = 2 * nbs + B - A + 1 + B + 1
    else:
        steps = 2 * nbs + B - A + 1
    return steps*pr




def test_all():
    gen_all_group()
    gen_pr()
    gen_all_strats()
    eval_all_strats()


def test_till_strats():
    gen_all_group()
    gen_pr()
    gen_all_strats()

def test_eval():
    eval(1, 0, 0.6)



sorted(l, k)




