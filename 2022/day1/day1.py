f = open("day1.txt")

totals = []
curr = 0

for x in f:
    if(x == "\n"):
        totals.append(curr)
        curr = 0
    else:
        curr += int(x)

totals.sort(reverse=True)
print(totals[0] + totals[1] + totals[2])