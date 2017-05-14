import re

class ABAFinder(object):
    def __init__(self, input):
        self.input = input
        self.window = 0
        self.end = len(input)

    def __iter__(self):
        return self

    def __next__(self):
        if (self.window + 2) == self.end:
            raise StopIteration()
        else:
            while (self.window + 2) < self.end:
                start = self.window
                end = start + 2
                self.window = self.window + 1
                if self.input[start] == self.input[end] and\
                self.input[start] != self.input[start+1]:
                    return self.input[start:end+1]
            raise StopIteration()

def has_bab(input, a, b):
    if len(input) < 3:
        return False

    for i in range(len(input)-2):
        if input[i] == b and input[i+1] == a and input[i+2] == b:
            return True
    return False

def is_bab_in_any_hypernet(addresses, a, b):
    for address in addresses:
        hypernet = address[1]
        if hypernet is not '' and has_bab(hypernet, a, b):
            #print('Hypernet: {}, BAB: {}'.format(hypernet, b+a+b))
            return True
    return False

def split_address(address):
    search = re.findall('(\w+)(\[\w+\])*', address)
    return search

if __name__ == '__main__':
    ssl_count = 0
    found = False
    with open('input.txt') as input_file:
        for line in input_file:
            addresses = split_address(line)

            for address in addresses:
                for aba in ABAFinder(address[0]):
                    a = aba[0]
                    b = aba[1]
                    # Check whether corresponding BAB is in any of the hypernets
                    if is_bab_in_any_hypernet(addresses, a, b):
                        #print('Address is: {} ABA is: {}'.format(address, aba))
                        ssl_count = ssl_count + 1
                        found = True
                        break
                if found:
                    found = False
                    break
    print('Total number of addresses supporting TLS: {}'.format(ssl_count))
