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


ROI = 10

count = 0
occu = set()
for (sx, sy, bx, by) in sensors:
    manhattan = abs(sx-bx)+abs(sy-by)

    if(abs(ROI-sy) > manhattan):
        pass

    vertical = abs(sy-ROI)
    horizontal = manhattan - vertical

    occu.update(range(sx-horizontal, sx+horizontal+1))
    #print(occu)
    # print((sx+horizontal,10), (sx-horizontal,10))

print(occu)
print(len(list(occu)))
