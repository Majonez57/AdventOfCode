with open("input.txt", 'r') as f:
    junctions = [[int(n) for n in j.split(',')] for j in f.read().splitlines()]

n = len(junctions)

def edge_dis(j1,j2):
    return ((j1[0]-j2[0])**2 + (j1[1]-j2[1])**2 + (j1[2]-j2[2])**2)**0.5

edges = sorted([(ji1,ji2) for ji1 in range(n) for ji2 in range(n) if ji2 > ji1], key=lambda edge: edge_dis(junctions[edge[0]],junctions[edge[1]]))

CONNECTIONS = 1000

circuits = [set(edges.pop(0))]

for ed in edges:

    start_graph = -1
    end_graph = -1
    for i in range(len(circuits)):
        circuit = circuits[i]
        if ed[0] in circuit:
            start_graph = i
        if ed[1] in circuit:
            end_graph = i
        if start_graph != -1 and end_graph != -1:
            break #We've found the components
    
    if start_graph == -1 and end_graph == -1: # Unconnected node
        circuits.append(set([ed[0], ed[1]])) # Add a new graph
    elif start_graph == end_graph: # Both nodes already contained in same component
        pass
    elif start_graph == -1:
        circuits[end_graph].add(ed[0])
    elif end_graph == -1:
        circuits[start_graph].add(ed[1])
    else:
        circuits.append(circuits.pop(max(start_graph, end_graph)) | circuits.pop(min(start_graph, end_graph)) | set(ed))
    
    if len(circuits[0]) == n:
        print(ed)
        print(junctions[ed[0]][0]*junctions[ed[1]][0])
        break
