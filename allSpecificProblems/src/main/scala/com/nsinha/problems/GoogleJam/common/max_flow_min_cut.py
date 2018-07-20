"""
Author: Nish Sinha

"""
import queue

nodes = [0, 1, 2, 3]
connections = [(0, 1, 4),(0,2,3), (1,3, 3), (1,2,2), (2,3,3)]

class Edge(object):
    def __init__(self):
        self.nodes_ids = [0,1]
        self.weight = 1

class Node(object):
    def __init__(self):
        self.edges_ids = []
        self.node_id = 0;

class FlowEdge(object):
    def __init__(self, cap, id):
        self.id = id
        self.capacity = cap
        self.ab = 0
        self.ba = 0

class MaxFlowMinCut(object):
    def __init__(self, nodes: list[Node], connections: list[(int,int,int)], src_node_id: int, dest_node_id: int):
        self.nodes_dict = {} #type: Dict[Node]
        self.edges_dict = {}
        self.nodes_to_edge = {}
        self.flow_edges_dict = {}
        self.src_node_id = src_node_id
        self.dest_node_id = dest_node_id

        for i in nodes:
            node = Node()
            self.nodes_dict[i] = node
            node.node_id = i

        edge_num = 0
        for (n1,n2,w) in connections:
            node_n1 = self.nodes_dict(n1) # type: Node
            node_n2 = self.nodes_dict(n2)
            node_n1.edges_ids.append(edge_num)
            node_n2.edges_ids.append(edge_num)
            edge = Edge()
            edge.nodes_ids = [node_n1.node_id, node_n2].sort()
            self.edges_dict[edge_num] = edge
            edge_num = edge_num + 1;
            edge.weight = w
            self.nodes_to_edge[tuple(edge.nodes_ids)] = edge_num

        self.form_augmented_graph();


    def form_augmented_graph(self):
        for edge_num in self.edges_dict:
            flow_edge = FlowEdge(self.edges_dict[edge_num].weight, edge_num)
            self.flow_edges_dict[edge_num] = flow_edge

    def solve_flow(self):
        max_flow = 0
        while (True):
            flow = self.find_a_path_to_dest_start()
            if (flow > 0):
                max_flow += flow
            else:
                break

        print("max_flow is {}", max_flow)


    def find_a_path_to_dest_start(self):
        self.visited = set()
        self.bfs_queue = queue.deque()
        self.bfs_queue.append((self.src_node_id, []))
        self.result_bfs = []
        if (self.do_bfs_till_dest_node() == -1):
            return -1
        else:
            flow = evaluate_flow()
            subtract_flow(flow)
            return flow




    def do_bfs_till_dest_node(self):
        if len(self.bfs_queue) == 0:
            return -1
        (next_node_id, present_path) = self.bfs_queue.remove()
        if (next_node_id == self.dest_node_id):
            self.result_bfs = present_path.append(next_node_id)
            return 0

        nbrs = self.find_not_full_unvisited_nbrs(next_node_id)
        [self.bfs_queue.append(nbr) for nbr in nbrs]
        return self.do_bfs_till_dest_node()



    def find_not_full_unvisited_nbrs(self, cur_node_id):
        cur_node = self.nodes_dict[cur_node_id] #type: Node
        results = []

        for i in cur_node.edges_ids:
            edge = self.edges_dict[i]
            flow_edge = self.flow_edges_dict[i] #type: FlowEdge
            other_node_id = set(edge.nodes_ids).difference(set(cur_node_id)).pop()
            if (self.visited.isdisjoint(set(other_node_id))):
                if (cur_node_id > other_node_id):
                    if (flow_edge.ba < (flow_edge.capacity+ flow_edge.ab)):
                        results.append(other_node_id)
                else:
                    if (flow_edge.ab < (flow_edge.capacity + flow_edge.ba)):
                        results.append(other_node_id)
            else:
                pass
        return results


    def evaluate_flow(self):
        last_node_id = None
        min_flow = 1e6
        for cur_node_id in self.result_bfs:
            flow = 1e6
            if (last_node_id is not None):
                if (last_node_id < cur_node_id):
                    edge_num = self.nodes_to_edge((last_node_id, cur_node_id))
                    flow_edge = self.flow_edges_dict[edge_num]
                    flow = flow_edge.capacity + flow_edge.ba - flow_edge.ab

                else:
                    edge_num = self.nodes_to_edge((cur_node_id, last_node_id))
                    flow_edge = self.flow_edges_dict[edge_num]
                    flow = flow_edge.capacity + flow_edge.ba - flow_edge.ab

                if (min_flow > flow):
                    min_flow = flow
            else:
                pass

        return min_flow




    def subtract_flow(self, flow):
        last_node_id = None
        for cur_node_id in self.result_bfs:
            if (last_node_id is not None):
                if (last_node_id < cur_node_id):
                    edge_num = self.nodes_to_edge((last_node_id, cur_node_id))
                    flow_edge = self.flow_edges_dict[edge_num]
                    flow_edge.ab += flow
                else:
                    edge_num = self.nodes_to_edge((ur_node_id, last_node_id))
                    flow_edge = self.flow_edges_dict[edge_num]
                    flow_edge.ba += flow
                if (min_flow > flow):
                    min_flow = flow
            else:
                pass

        return


