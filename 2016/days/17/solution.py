import hashlib
from collections import deque

PASSCODE = 'qtetzkpl'

class Node(object):
    def __init__(self, coords, path):
        self.coords = coords
        self.path = path

def is_open(character):
    return character in ['b', 'c', 'd', 'e', 'f']

def get_allowed_movements(code, coords):
    allowed = []
    md5 = hashlib.md5()
    md5.update(code.encode('utf-8'))
    hexdigest = md5.hexdigest()
    x, y = coords

    if is_open(hexdigest[0]) and y != 0:
        allowed.append('U')
    if is_open(hexdigest[1]) and y != 3:
        allowed.append('D')
    if is_open(hexdigest[2]) and x != 0:
        allowed.append('L')
    if is_open(hexdigest[3]) and x != 3:
        allowed.append('R')

    return allowed

def get_coords(curr_coords, movement):
    x, y = curr_coords

    if movement == 'U':
        y = y - 1
    elif movement == 'R':
        x = x + 1
    elif movement == 'D':
        y = y + 1
    elif movement == 'L':
        x = x - 1

    return (x, y)

def is_vault_room(coords):
    return coords == (3, 3)

def find_shortest_path():
    queue = deque()
    node = Node((0, 0), [])
    not_in_vault_room = True

    queue.appendleft(node)

    while not_in_vault_room and len(queue) > 0:
        node = queue.pop()
        coords = node.coords
        path = node.path
        allowed_movements = get_allowed_movements(PASSCODE + ''.join(path), coords)

        for movement in allowed_movements:
            new_node = Node(get_coords(coords, movement), path + [movement])
            queue.appendleft(new_node)

            if ''.join(new_node.path) == 'D':
                pass

            if is_vault_room(new_node.coords):
                return new_node

    return None

if __name__ == '__main__':
    node = find_shortest_path()
    print('Shortest path is: {}'.format(''.join(node.path)))
