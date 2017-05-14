class Decompressor(object):
    # Allowed modes
    TOKENIZE = 1
    REPEAT = 3
    RANGE = 4
    DECOMPRESS = 5

    def parse(self, character):
        """
        Read character and perform actions based on current mode.
        """
        if self.mode is Decompressor.TOKENIZE:
            if character is '(':
                self.mode = Decompressor.RANGE
                self.range = ''
                self.range_text = ''
            else:
                self.decompressed_text = self.decompressed_text + character
        elif self.mode is Decompressor.RANGE:
            if character is 'x':
                self.mode = Decompressor.REPEAT
                self.repeat = ''
                self.range = int(self.range)
            else:
                self.range = self.range + character
        elif self.mode is Decompressor.REPEAT:
            if character is ')':
                self.mode = Decompressor.DECOMPRESS
                self.range_text = ''
                self.repeat = int(self.repeat)
            else:
                self.repeat = self.repeat + character
        elif self.mode is Decompressor.DECOMPRESS:
            if self.range > 0:
                self.range_text = self.range_text + character
                self.range = self.range - 1
            if self.range == 0:
                self.mode = Decompressor.TOKENIZE
                self.decompressed_text = self.decompressed_text + (self.range_text * self.repeat)

    def decompress(self):
        with open(self.filename) as file:
            for line in file:
                for character in line:
                    if not character.isspace():
                        self.parse(character)

        return self.decompressed_text

    def __init__(self, filename):
        self.filename = filename
        self.repeat = None
        self.range = None
        self.range_text = None
        self.mode = Decompressor.TOKENIZE
        self.decompressed_text = ""

if __name__ == '__main__':
    decompressor = Decompressor('input.txt')
    text = decompressor.decompress()

    print('Decompressed text is {} characters long.'.format(len(text)))
    #print('Text is: {}'.format(text))