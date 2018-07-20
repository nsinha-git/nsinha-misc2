**Problem of food = 1**

1. Order the spells(S) according to lowest output. Lowest output comes first.
2. What is two things have lowest output. If one has higher input then that comes
    1. if earlier:
        (5,2) (3,2) leads to +2 -2 +2 = 2
    2. If later:
        (3,2) (5,2) leads to +2 -2 +2 = 2
    it does not matter.
3. So a clean algo is order by o/p and that's it.

**Problem of food =2**
0. Treat the 2 food as group and reduce the problem to 1.
1. Solve it as like 1.
2. But when coming to case like:
    S1(3,2|2,2) , S2(4,2|2,2)
    1. S1 vs S2:







