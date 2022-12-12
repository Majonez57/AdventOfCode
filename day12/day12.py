f = open('day12.txt').read().strip().split('\n')

width = len(f[0])
height = len(f)

stack = []
hMap = []
travel = dict()

lows = []

for i in range(height):
    line  = f[i]
    hLine = []
    for j in range(width):
        char = line[j]
        if char == 'S':
            start = (i,j)
            char = 'a'
        elif char == 'E':
            end = (i,j)
            char = 'z'
            
        if char == 'a':
            lows.append((i,j))
            
        hLine.append(ord(char) - ord('a'))
        stack.append((i,j))
    hMap.append(hLine)

for i in range(height):
    for j in range(width):
        toVisit = []
        for m in [-1,0,1]: #Up same down
            for n in [-1,0,1]:
                if 0 <= i+m < height and 0 <= j+n < width and abs(n)+abs(m) == 1:
                    if hMap[i+m][j+n] - hMap[i][j] <= 1:
                        toVisit.append((i+m, j+n))
        travel[(i,j)] = toVisit

class bfs:
    def __init__(self, stack, visited, start, end, nmap):
        self.stack = stack
        self.visited = visited
        self.start = start
        self.end = end
        self.nmap = nmap
        self.nDis = dict(zip(stack, [999]*(height*width)))
        self.nDis[start] = 0
    
    def visit(self, curr):
        for node in self.nmap[curr]:
            if node not in self.visited:
                if self.nDis[curr] + 1 < self.nDis[node]:
                    self.nDis[node] = self.nDis[curr] + 1
        self.stack.remove(curr)
        self.visited.append(curr)
    
    def doBfs(self):
        while self.end not in self.visited:
            curr = min(self.stack, key=self.nDis.get)
            self.visit(curr)
        return self.nDis[self.end]

s = stack.copy()
h = bfs(s, [], start, end, travel)
print(h.doBfs())

res = []
for i in lows:
    s = stack.copy()
    h = bfs(s, [], i, end, travel).doBfs()
    res.append(h)
    print(h)

print(min(res))