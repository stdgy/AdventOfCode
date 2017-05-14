
def populate_list(total):
    nums = []
    for num in range(total):
        nums.append(num)
    return nums

def process_gift_steals(elves):
    is_stolen_from = False
    while len(elves) > 1:
        remaining = []
        for elf in elves:
            if not is_stolen_from:
                remaining.append(elf)
                is_stolen_from = True
            else:
                is_stolen_from = False
        elves = remaining
    return elves

if __name__ == '__main__':
    elves = populate_list(3012210)
    elves = process_gift_steals(elves)
    print('The remaining elf is in position {}'.format(elves[0]+1))