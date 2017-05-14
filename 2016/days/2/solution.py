class Keypad(object):

    def move_up(self):
        if self.current_pos[1] != 0:
            self.current_pos[1] = self.current_pos[1] - 1

    def move_down(self):
        if self.current_pos[1] != 2:
            self.current_pos[1] = self.current_pos[1] + 1


    def move_left(self):
        if self.current_pos[0] != 0:
            self.current_pos[0] = self.current_pos[0] - 1

    def move_right(self):
        if self.current_pos[0] != 2:
            self.current_pos[0] = self.current_pos[0] + 1

    def get_key(self):
        return (3 * self.current_pos[1]) + self.current_pos[0] + 1

    def __init__(self):
        self.current_pos = [1, 1]

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
    