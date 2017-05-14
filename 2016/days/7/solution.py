import re

def is_abba(text):
    """
    :type text: str
    Returns whether a text string is an ABBA string.
    """
    if len(text) < 4:
        return False

    for window in range(len(text)-3):
        if text[window] == text[window + 3] and\
        text[window + 1] == text[window + 2] and\
        text[window] != text[window+1]:
            print(text[window:window+4])
            return True

    return False

def split_address(address):
    search = re.findall('(\w+)(\[\w+\])*', address)
    return search

if __name__ == '__main__':
    tls_count = 0
    with open('input.txt') as input_file:
        for line in input_file:
            line_is_abba = False
            for address in split_address(line):
                ip = address[0]
                hypernet = address[1]

                line_is_abba = (line_is_abba or is_abba(ip))

                if is_abba(hypernet):
                    line_is_abba = False
                    break

            if line_is_abba is True:
                tls_count = tls_count + 1

    print('Total number of addresses supporting TLS: {}'.format(tls_count))
