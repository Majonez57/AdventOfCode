with open('input.txt', 'r') as f:
    homework = f.read().splitlines()

def part_one():
    homework = [[n for n in x.split(' ') if n != ''] for x in homework]

    total = 0 
    #print(homework[0])
    for n in range(len(homework[0])):
        op = homework[-1][n]
        if op == '*': 
            f = lambda acc, x: acc*x
            accu = 1
        else: 
            f = lambda acc, x: acc+x
            accu = 0
        [accu := f(accu, int(homework[i][n])) for i in range(len(homework)-1)]
        total += accu 
    print(total)

def part_two():
    operators = [x for x in homework[-1].split() if x != ' ']
    numT = list(zip(*homework[:-1]))
    numT.append((' ',' ',' ',' '))
    
    total = 0
    curr = []
    op_i = 0
    for i in numT:
        if i == (' ',' ',' ',' '):
            if operators[op_i] == '*':
                f = lambda acc, x: acc*x
                accu = 1
            else: 
                f = lambda acc, x: acc+x
                accu = 0
            
            
            [accu := f(accu, n) for n in curr]

            print(curr, operators[op_i], accu)
            total += accu 

            op_i += 1
            curr = []
        else:
            curr.append(int(''.join(list(i))))

    print(total)

part_two()