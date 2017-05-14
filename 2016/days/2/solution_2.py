class Keypad(object):

    def move_up(self):
        if self.row > 0 and self.mask[self.row-1][self.col] != 'x':
            self.row = self.row - 1

    def move_down(self):
        if self.row < 4 and self.mask[self.row+1][self.col] != 'x':
            self.row = self.row + 1

    def move_left(self):
        if self.col > 0 and self.mask[self.row][self.col-1] != 'x':
            self.col = self.col - 1

    def move_right(self):
        if self.col < 4 and self.mask[self.row][self.col+1] != 'x':
            self.col = self.col + 1

    def get_key(self):
        return self.mask[self.row][self.col]

    def __init__(self):
        self.row = 2
        self.col = 0

        self.mask = [
            'xx1xx',
            'x234x',
            '56789',
            'xabcx',
            'xxdxx'
        ]

if __name__ == "__main__":
    keypad = Keypad()
    with open('input.txt') as input_file:
        for line in input_file:
            for token in line:
                if token == "U":
                    keypad.move_up()
                elif token == "D":
                    keypad.move_down()
                elif token == "R":
                    keypad.move_right()
                elif token == "L":
                    keypad.move_left()
            print("{}".format(keypad.get_key()))
    