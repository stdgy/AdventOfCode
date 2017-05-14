import re

class Disc(object):
    """
    Disc iterator -- Lets us loop through valid
    pass through values. That is, times when the disc
    position is 0.
    """
    def __init__(self, positions, position, time, order):
        self.positions = positions
        self.position = position
        self.time = time
        self.curr_time = 0
        self.order = order

    def __iter__(self):
        self.curr_time = self.time
        return self

    def __next__(self):
        # Return times when (position + time) % positions = 0
        if self.position + self.curr_time < self.positions:
            self.curr_time = self.positions - self.position
        else:
            self.curr_time = self.curr_time + self.positions
        return self.curr_time - self.order

    def is_a_drop_time(self, time):
        return (time + self.time + self.order + self.position) % self.positions == 0

def parse_disc(line):
    integers = re.findall('\d+', line)
    order, positions, time, position = map(int, integers)
    return Disc(positions, position, time, order)

def time_works_for_discs(test_time, discs):
    for disc in discs:
        if disc.is_a_drop_time(test_time) == False:
            return False
    return True

def get_min_drop_time(discs):
    first_disc = discs[0]
    other_discs = discs[1:]

    for time in first_disc:
        if time_works_for_discs(time, other_discs):
            return time

if __name__ == '__main__':
    discs = []

    # Read in all of the discs
    with open('test_input.txt') as file:
        for line in file:
            discs.append(parse_disc(line))

    # Calculate first time all we can insert at such that the object
    # will fall through all of the discs
    min_drop_time = get_min_drop_time(discs)
    print('Minimum drop time is: {}'.format(min_drop_time))
