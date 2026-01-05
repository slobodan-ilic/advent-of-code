with open("input.txt", "r") as f:
    lines = f.readlines()

dial = 50
n_zeros = 0

for line in lines:
    direction = line[0]
    distance = int(line[1:].strip())
    if direction == 'R':
        dial = dial + distance % 100
    elif direction == 'L':
        dial = dial - distance % 100
    
    if dial < 0:
        dial = dial + 100
    elif dial > 99:
        dial = dial - 100
    
    print(dial)
    
    if dial == 0:
        n_zeros = n_zeros + 1

print(n_zeros)