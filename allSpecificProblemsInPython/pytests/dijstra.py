from queue import Queue

graph = dict()

graph["v1"] = [("v2",2), ("v3", 4)]
graph["v2"] = [("v3",3), ("v4", 7)]
graph["v3"] = [("v4",2), ("v5", 1)]
graph["v4"] = []
graph["v5"] = []

cost_map = {}
p_que = Queue()
visited = set()


def dijstra(src, dest):
    cost_map[src] = 0
    p_que.put(src)
    visited = set()
    dijstra_real()
    print(str(cost_map.get(dest)))

def dijstra_real():
    if p_que.empty():
        return
    el = p_que.get()
    if el in visited:
        return dijstra_real()
    nbrs_tuples = graph[el]
    cur_cost = cost_map[el]
    for nbrs_tuple in nbrs_tuples:
        nbr_cost = cost_map.get(nbrs_tuple[0])
        if (nbr_cost is not None):
            if (nbr_cost > cur_cost + nbrs_tuple[1]):
                cost_map[nbrs_tuple[0]] = cur_cost + nbrs_tuple[1]
        else:
            cost_map[nbrs_tuple[0]] = cur_cost + nbrs_tuple[1]

        if not (nbrs_tuple[0] in visited):
            p_que.put(nbrs_tuple[0])
    visited.add(el)
    return dijstra_real()

dijstra("v1","v4")