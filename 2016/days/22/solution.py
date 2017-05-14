import re

class Node(object):
    def avail(self):
        return self.size - self.used

    def __init__(self, x, y, used, size):
        self.x = x
        self.y = y
        self.used = used
        self.size = size

def is_viable_pair(a, b):
    """
    :type a: Node
    :type b: Node
    """
    if (a.x, a.y) != (b.x, b.y) and a.used > 0 and b.avail() > a.used:
        return True
    return False

def parse_node(line):
    x, y = re.findall('x(\d+)-y(\d+)', line)[0]
    size, used = re.findall('(\d+)T\s+(\d+)T\s+\d+T\s+\d+%', line)[0]

    return Node(int(x), int(y), int(used), int(size))

if __name__ == '__main__':
    num_viable = 0
    nodes = []
    with open('input.txt') as file:
        file.readline()
        file.readline()

        for line in file:
            nodes.append(parse_node(line))

    for node in nodes:
        for other_node in nodes:
            if is_viable_pair(node, other_node):
                num_viable = num_viable + 1

    print('Number of viable pairs: {}'.format(num_viable))