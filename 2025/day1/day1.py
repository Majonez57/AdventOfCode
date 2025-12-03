
def mod(a,b):
    return (a%b+b)%b

with open("input.txt") as f:
    steps = f.read().splitlines()

dial = 50

count = 0
for step in steps:
    direction = -1 if step[0] == 'L' else 1
    magnetude = int(step[1:])
    
    count += abs(magnetude//100) # If input is >100
    magnetude = magnetude % 100
    
    old = dial
    dial += direction*magnetude
    ndial = mod(dial, 100)

    if direction == 1 and dial > 100 and old != 0: #FORWARD
        count += 1
        print("tickf")
        
    elif direction == -1 and dial < 0 and old != 0: #BACKWARDS
        count += 1
        print("tickb")
    
    if ndial == 0:
        count += 1
        print("tickl")

    dial=ndial
    print(ndial)

print(count)