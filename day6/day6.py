

curr = ""
total = 0
f = open("day6.txt").read()

for i in f:
    print(curr)
    curr += i
    total += 1
    if(len(curr) > 14): # Change to 4 for part 1
        curr = curr[1:]
        if(len(curr) == len(set(curr))):
            print(total)
            break
        
