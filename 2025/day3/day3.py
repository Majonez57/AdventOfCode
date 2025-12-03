import timeit

with open("input-p1.txt", 'r') as f:
    banks = f.read().splitlines()

def part_1():
    total = 0
    for bank in banks:
        curr = 0
        sec = 0

        for i in range(len(bank)):
            n = int(bank[i])
            if n > curr and i != len(bank)-1:
                curr = n
                sec = 0 
            elif n > sec:
                sec = n

        #print(int(curr)*10 + int(sec))
        total += int(curr)*10 + int(sec)

    #print(total)

def part_2():
    total = 0

    for bank in banks:
        current = [0,0,0,0,0,0,0,0,0,0,0,0]
        banksize = len(bank)
        for i in range(banksize): 
            n = int(bank[i]) # Current digit.
            remaining = banksize - i # Number of digits left to read
            
            # The idea is to 'float' the digit to the left if there is a 0
            # Then we replace a digit at n if there are at least 12-n digits remaining

            
            spot = 0 if remaining > 12 else 12-remaining
            #print(n)
            for j in range(spot,12):
                if current[j] < n:
                    current[j] = n
                    for k in range(j+1, 12) : current[k] = 0
                    break

            #print(current)
        total += int(''.join([str(x) for x in current]))
    
    #print(total)

print(timeit.timeit(part_2, number=100))