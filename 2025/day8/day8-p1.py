import math

with open("input.txt", 'r') as f:
    junctions = [[int(n) for n in j.split(',')] for j in f.read().splitlines()]

n = len(junctions)

def edge_dis(j1,j2):
    return ((j1[0]-j2[0])**2 + (j1[1]-j2[1])**2 + (j1[2]-j2[2])**2)**0.5

edges = sorted([(ji1,ji2) for ji1 in range(n) for ji2 in range(n) if ji2 > ji1], key=lambda edge: edge_dis(junctions[edge[0]],junctions[edge[1]]))

CONNECTIONS = 1000

circuits = []
seen = []

def get_connected(edge):
    if edge in seen: return []
    seen.append(edge)

    connected = set([*edge])

    for e in edges:
        ns, ne = e[0], e[1]
        if ns in connected or ne in connected: 
            connected.update(get_connected(e))

    return connected

edges = edges[:CONNECTIONS]
res = sorted([len(get_connected(e)) for e in edges])[-3:]
print(math.prod(res))