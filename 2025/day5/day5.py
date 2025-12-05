import timeit

with open("input-d5.txt", 'r') as f:
    all_in = f.read().splitlines()


delimeter_i = all_in.index('')
ranges = sorted([[int(n) for n in r.split('-')] for r in all_in[:delimeter_i]])
items = [int(n) for n in all_in[delimeter_i+1:]]

def part_one():
    delimeter_i = all_in.index('')
    ranges = sorted([[int(n) for n in r.split('-')] for r in all_in[:delimeter_i]])
    items = [int(n) for n in all_in[delimeter_i+1:]]

    count = 0
    for item in items:
        # check if we are in one of the ranges
        for rmin,rmax in ranges:
            if item >= rmin and item <= rmax:
                count +=1
                break
    print(count)

def part_two():
    # Need to combine all the ranges to be contigous
    # I'm gonna use my janky ahh method I abandoned on d3
    delimeter_i = all_in.index('')

    ranges = sorted([[int(n) for n in r.split('-')] for r in all_in[:delimeter_i]])

    new_ranges = []
    i = 0
    while i < len(ranges):
        cmin = ranges[i][0]
        cmax = ranges[i][1]
        #print('c:', cmin, cmax)
        while True: #Check what subsequent ranges have any crossover. Break once a gap is found
            if i == len(ranges)-1: break # No more ranges left
            # Next range in the list
            nmin = ranges[i+1][0]
            nmax = ranges[i+1][1]
            #print('n:', nmin, nmax)

            if cmax < nmin : break # A gap is found!
            else: # There is crossover
                if nmin < cmin: cmin = nmin # If the next min is smaller than the current min
                if nmax > cmax: cmax = nmax
                i += 1 #Next range has been absorbed so we don't need to check it
                
        new_ranges.append([cmin, cmax])
        i += 1
        
    print(sum([1+rmax-rmin for (rmin, rmax) in new_ranges]))

print(timeit.timeit(part_one, number=100)/100)
print(timeit.timeit(part_two, number=100)/100)