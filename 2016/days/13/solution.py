from math import ceil
from collections import deque
# Represent graph as adjacency matrix
# Use Djikstra's to get shortest path

PUZZLE_INPUT = 1362
INFINITE = None

def is_wall(x, y):
    s = (x * x) + (3 * x) + (2 * x * y) + y + (y * y)
    s = s + PUZZLE_INPUT
    b = s.to_bytes(ceil(s.bit_length() / 8), byteorder='big')
    num_ones = 0

    for byte in b:
        num_ones = num_ones + (byte & 0b1)
        num_ones = num_ones + ((byte >> 1) & 0b1)
        num_ones = num_ones + ((byte >> 2) & 0b1)
        num_ones = num_ones + ((byte >> 3) & 0b1)
        num_ones = num_ones + ((byte >> 4) & 0b1)
        num_ones = num_ones + ((byte >> 5) & 0b1)
        num_ones = num_ones + ((byte >> 6) & 0b1)
        num_ones = num_ones + ((byte >> 7) & 0b1)

    return num_ones % 2 == 1

def draw_maze(width, height):
    for x in range(width):
        for y in range(height):
            if is_wall(x, y):
                print('#', end='')
            else:
                print('.', end='')
        print()

def initialize_shortest_path_grid(width, height):
    """
    Initialize a grid that stores the shortest path to each coordinate.
    Begin by initializing all coordinates to infinity.
    """
    grid = []
    for x_coord in range(width):
        row = []
        for y_coord in range(height):
            row.append(INFINITE)
        grid.append(row)
    return grid

def calculate_shortest_paths(shortest_grid, start_x, start_y):
    """
    Calculate shortest path to all reachable points using Dijkstra's Algorithm
    """
    height = len(shortest_grid)
    width = len(shortest_grid[0])

    # Set starting point to 0
    shortest_grid[start_y][start_x] = 0

    q = deque()
    q.appendleft((start_x, start_y))

    while len(q) > 0:
        x, y = q.pop()
        shortest_path = shortest_grid[y][x]
        connected_nodes = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

        for node in connected_nodes:
            node_x, node_y = node
            if node_x >= 0 and node_x < width and node_y >= 0 and node_y < height\
            and not is_wall(node_x, node_y):
                curr_shortest_path = shortest_grid[node_y][node_x]
                if curr_shortest_path == INFINITE or (shortest_path + 1) < curr_shortest_path:
                    shortest_grid[node_y][node_x] = shortest_path + 1
                    q.appendleft((node_x, node_y))

def print_shortest_path(shortest_path_grid):
    for row in shortest_path_grid:
        for col in row:
            if col is None:
                print('|{:3}'.format(''), end='')
            else:
                print('|{:3}'.format(col), end='')
        print()


if __name__ == '__main__':
    shortest_path_grid = initialize_shortest_path_grid(50, 50)
    calculate_shortest_paths(shortest_path_grid, 1, 1)
    print_shortest_path(shortest_path_grid)
    print('Shortest number of steps to (31,39) is {} steps'.format(shortest_path_grid[39][31]))