class Screen(object):
    def count_lit_pixels(self):
        count = 0
        for row in self.monitor:
            for val in row:
                if val == '1':
                    count = count + 1
        return count

    def display(self):
        for row in self.monitor:
            print(''.join(row))

    def draw(self, width, height):
        for x in range(height):
            for y in range(width):
                self.monitor[x][y] = '1'

    def shift_row_right(self, row, shift):
        r = self.monitor[row]
        self.monitor[row] = self._shift_list(r, shift)

    def shift_col_down(self, col, shift):
        # Copy column into a row to make it easier to deal with
        new_column = self._copy_column(col)
        # Shift values in copied column
        new_column = self._shift_list(new_column, shift)
        # Put the modified values back into their home column
        for idx, val in enumerate(new_column):
            self.monitor[idx][col] = val

    def _shift_list(self, l, shift_amount):
        new_list = []
        list_length = len(l)
        for idx in range(list_length):
            val = l[(idx - shift_amount) % list_length]
            new_list.append(val)
        return new_list

    def _copy_column(self, col):
        column = []
        for row in range(len(self.monitor)):
            column.append(self.monitor[row][col])
        return column

    def __init__(self):
        self.monitor = []

        # Initialize monitor to all off
        for row in range(6):
            row = []
            for col in range(50):
                row.append('0')
            self.monitor.append(row)

def parse_rect(line):
    dimensions = line[4:]
    dimensions = dimensions.split('x')
    dimensions = map(int, dimensions)
    return dimensions

def parse_rotate_row(line):
    rotate_params = line[13:]
    rotate_params = rotate_params.split('by')
    rotate_params = map(str.strip, rotate_params)
    rotate_params = map(int, rotate_params)
    return rotate_params

def parse_rotate_col(line):
    rotate_params = line[16:]
    rotate_params = rotate_params.split('by')
    rotate_params = map(str.strip, rotate_params)
    rotate_params = map(int, rotate_params)
    return rotate_params

if __name__ == '__main__':
    s = Screen()

    with open('input.txt') as file:
        for line in file:
            if 'rect' == line[:4]:
                width, height = parse_rect(line)
                s.draw(width, height)
            elif 'rotate row' == line[:10]:
                row, shift = parse_rotate_row(line)
                s.shift_row_right(row, shift)
            elif 'rotate column' == line[:13]:
                col, shift = parse_rotate_col(line)
                s.shift_col_down(col, shift)

    s.display()

    print('There are {} pixels lit on the display.'.format(s.count_lit_pixels()))

