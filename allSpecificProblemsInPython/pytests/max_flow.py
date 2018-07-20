from functools import reduce

'''



'''

nodes = [0,1,2,3,4,5]
edges =[(0,1,16), (0,2,13), (1,3,12), (1,2, 4), (2,3,9), (2,4,14), (3,4,7), (3,5,20),(4,5,4)]
list_edges = []
aug_edges = []
flow = 0

def create_augmented_edgges(edges):
    global aug_edges
    for e in edges:
        e1 = (e[0],e[1],e[2],0,0)
        aug_edges.append(e1)

def create_edge_map():
    for v in nodes:
        list_edges.append([])

    for i,e in zip(range(0,len(edges)),edges):
        v1 = e[0]
        v2 = e[1]
        list_edges[v1].append(i)
        list_edges[v2].append(i)

create_augmented_edgges(edges)
create_edge_map()
print(aug_edges)
print(list_edges)

def cal_flow():
    global flow
    while do_dfs(0,5) == 1:
        flow += 1

    print(flow)

def do_dfs(src = 0, dest= 5, visited = set()):
    nbrs_edges = list_edges[src] #[[0, 1], [0, 2, 3], [1, 3, 4, 5], [2, 4, 6, 7], [5, 6, 8], [7, 8]]
    appl_nbrs_edges = applicable_nbrs_edges(src, nbrs_edges) #[0,1,2]
    acc = set()
    for appl_nbrs_edge in appl_nbrs_edges:
        acc.add(edges[appl_nbrs_edge][0])
        acc.add(edges[appl_nbrs_edge][1])
    acc.remove(src)
    set_of_nbr_vertices = acc
    set_of_nbr_vertices = set_of_nbr_vertices.difference(visited)

    if dest in set_of_nbr_vertices:
        return update_edges_passing_thru_node(appl_nbrs_edges, src, dest)

    if len(set_of_nbr_vertices) == 0:
        return 0

    visited.add(src)

    for i in set_of_nbr_vertices:
        t = do_dfs(i, dest, visited.copy())
        if (t == 1):
            return update_edges_passing_thru_node(appl_nbrs_edges, src, i)
    return 0

def update_edges_passing_thru_node(appl_nbrs_edges, src, dest):
    global aug_edges
    for edge in appl_nbrs_edges:
        if edges[edge][0] == dest and edges[edge][1] == src:
            aug_edges[edge] = (aug_edges[edge][0],aug_edges[edge][1], aug_edges[edge][2], aug_edges[edge][3], aug_edges[edge][4] + 1)
            return 1
        if edges[edge][0] == src and edges[edge][1] == dest:
            aug_edges[edge] = (aug_edges[edge][0],aug_edges[edge][1], aug_edges[edge][2], aug_edges[edge][3] + 1, aug_edges[edge][4])
            return 1


def applicable_nbrs_edges(src, edges): # (0, [0, 1])
    global aug_edges
    applicable_edges = []
    for e in edges:
        aug_edge = aug_edges[e] #(0, 1, 16, 0, 0),
        if src == aug_edge[0]:
            if aug_edge[3] < aug_edge[4] + aug_edge[2]:
                applicable_edges.append(e)
        else:
            if src == aug_edge[1]:
                if aug_edge[4] < aug_edge[3] + aug_edge[2]:
                    applicable_edges.append(e)

    return applicable_edges


cal_flow()

