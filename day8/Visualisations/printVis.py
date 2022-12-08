import matplotlib.pyplot as plt
import math

f = open ("completeScenes.txt").read().splitlines()

for i in range(0, len(f)-1):
    f[i] = f[i].split(',')
    
data = []

for i in f:
    curr = []
    for j in i:
        if(j.isdigit()):
            if(j == '0'):
                curr.append(0)
            else:
                curr.append(int(j))
    data.append(curr)

data = data [:-1]
plt.imshow(data, cmap='viridis')
plt.savefig("visual3")