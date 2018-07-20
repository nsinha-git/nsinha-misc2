from sortedcollections import SortedList,SortedDict,SortedListWithKey,SortedSet
from queue import Queue

graph = {}

graph["v1"] = ["v2","v3"]
graph["v2"] = ["v3","v4"]
graph["v3"] = ["v4","v5"]
graph["v4"] = []
graph["v5"] = []
graph["v6"] = ["v7"]
graph["v7"] = []
'''
a parent should come before its child
a loop causes impposibility cycle. cycle_check_set should be maintained.
if a node is already visted we should not visit that again
'''
res = list()
visited = set()
graph_nodes = Queue()
for node in graph.keys():
    graph_nodes.put(node)
cycle = False

def topo_sort():

    while not graph_nodes.empty():
        el = graph_nodes.get()
        if not el in visited:
            res.append(Queue())
            do_dfs(el, set())

    print(res)



def do_dfs(el, cycle_set):
    if el in cycle_set:
        cycle = True
        return

    cycle_set.add(el)
    visited.add(el)

    nbrs = graph[el]
    for nbr in nbrs:
        if not nbr in visited:
            do_dfs(nbr, cycle_set)
    res[len(res) - 1].put(el)

    return


topo_sort()

