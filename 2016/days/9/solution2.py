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
                self.decompressed_length = self.decompressed_length + 1
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
                # Recursively get length of decompressed text of this section
                sub_decompressor = Decompressor(self.range_text)
                sub_length = sub_decompressor.get_decompressed_length()
                self.mode = Decompressor.TOKENIZE
                self.decompressed_length = self.decompressed_length + (sub_length * self.repeat)

    def get_decompressed_length(self):
        for character in self.text:
            if not character.isspace():
                self.parse(character)

        return self.decompressed_length

    def __init__(self, text):
        self.text = text
        self.repeat = None
        self.range = None
        self.range_text = None
        self.mode = Decompressor.TOKENIZE
        self.decompressed_length = 0

if __name__ == '__main__':
    text = ''
    with open('input.txt') as file:
        text = file.read()
    decompressor = Decompressor(text)
    length = decompressor.get_decompressed_length()

    print('Decompressed text is {} characters long.'.format(length))
    #print('Text is: {}'.format(text))