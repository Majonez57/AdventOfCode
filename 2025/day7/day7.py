with open("test.txt", 'r') as f:
    teleporter = f.read().splitlines()


tachyons = [0] * len(teleporter[0])
tachyons[teleporter[0].index('S')] = 1

for lin in teleporter[1:]:
    for i in range(len(teleporter[0])):
        sym = lin[i]
        if sym == '^' and tachyons[i] > 0:
            tachyons[i+1] += tachyons[i]
            tachyons[i-1] += tachyons[i]
            tachyons[i] = 0

print(tachyons)
print(sum([n for n in tachyons if n != 0]))
print(sum(tachyons))
