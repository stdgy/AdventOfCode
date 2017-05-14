import math

PUZZLE_INPUT=3012210

def populate_list(total):
    nums = []
    for num in range(total):
        nums.append(num)
    return nums

def get_midpoint(total):
    return math.floor(total/2)

def get_next(elves, index, do_skip):
    index = (index + 1) % len(elves)
    while elves[index] == None:
        index = (index + 1) % len(elves)

    if do_skip:
        index = (index + 1) % len(elves)
        while elves[index] == None:
            index = (index + 1) % len(elves)

    return index

def get_remaining_elf(elves):
    for elf in elves:
        if elf is not None:
            return elf
    return None

def process_gift_steals(elves):
    current_elf = get_midpoint(len(elves))
    remaining = len(elves)
    do_skip = (len(elves) % 2) == 1
    while remaining > 1:
        elves[current_elf] = None
        current_elf = get_next(elves, current_elf, do_skip)

        if do_skip:
            do_skip = False
        else:
            do_skip = True

        remaining = remaining - 1

    return elves

if __name__ == '__main__':
    elves = populate_list(PUZZLE_INPUT)
    elves = process_gift_steals(elves)
    remaining_elf = get_remaining_elf(elves)
    print('The remaining elf is in position {}'.format(remaining_elf + 1))