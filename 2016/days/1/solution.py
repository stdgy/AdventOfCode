"""
Solution to the first day of the advent calendar programming challenge.
"""
class Santa(object):
    """
    Santa class encapsulates the movement and position tracking.
    """
    def turn_right(self):
        """
        Turns santa right.
        """
        if self.facing is "NORTH":
            self.facing = "WEST"
        elif self.facing is "WEST":
            self.facing = "SOUTH"
        elif self.facing is "SOUTH":
            self.facing = "EAST"
        else:
            self.facing = "NORTH"

    def turn_left(self):
        """
        Turns santa left, changing which direction he is facing.
        """
        if self.facing is "NORTH":
            self.facing = "EAST"
        elif self.facing is "WEST":
            self.facing = "NORTH"
        elif self.facing is "SOUTH":
            self.facing = "WEST"
        else:
            self.facing = "SOUTH"

    def has_visited_this_spot(self):
        return (self.x, self.y) in self.visited_locations
    
    def record_location(self):
        self.visited_locations[(self.x, self.y)] = 1

    def move(self, steps):
        """
        Moves santa in a number of steps in the direction he is facing.
        """
        for i in range(steps):
            if self.facing is "NORTH":
                self.y = self.y + 1
            elif self.facing is "EAST":
                self.x = self.x + 1
            elif self.facing is "SOUTH":
                self.y = self.y - 1
            else:
                self.x = self.x - 1
            
            if self.has_visited_this_spot():
                raise RuntimeError()
            
            self.record_location()


    def __init__(self):
        self.x = 0
        self.y = 0
        self.facing = 'NORTH'
        self.visited_locations = {}

if __name__ == "__main__":
    santa = Santa()
    with open("./input.txt") as f:
        input_tokens = f.read().replace(" ", "").strip().split(",")
        for tok in input_tokens:
            if tok[:1] is "R":
                santa.turn_right()
            else:
                santa.turn_left()

            try:
                santa.move(int(tok[1:]))
            except RuntimeError:
                print("Visited this spot before: ({}, {})".format(santa.x, santa.y))
                print("Total Spaces Away: {}".format(abs(santa.x) + abs(santa.y)))

    print("X: {}, Y: {}".format(santa.x, santa.y))
    print("Total Spaces Away: {}".format(abs(santa.x) + abs(santa.y)))

        