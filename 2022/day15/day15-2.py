

with open("test.txt") as f:
    lines = f.read().splitlines()

sensors = []
for line in lines:
    linesplit = line.split('=')
    
    sensors.append(
        (int(linesplit[1].split(',')[0]), 
         int(linesplit[2].split(':')[0]),
         int(linesplit[3].split(',')[0]),
         int(linesplit[4]))
    )



for i in range(0,4000000):
    for j in range(0,4000000):
        pass
