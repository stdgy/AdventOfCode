DISK_LENGTH = 35651584
INITIAL_DATA = '00111101111101000'

def reverse(string):
    reversed = []
    for character in string:
        if character == '0':
            reversed.append('1')
        else:
            reversed.append('0')
    reversed.reverse()
    return ''.join(reversed)

def expand(data, max_length):
    expanded = data + '0' + reverse(data)

    while len(expanded) < max_length:
        expanded = expanded + '0' + reverse(expanded)

    if len(expanded) > max_length:
        expanded = expanded[0:max_length]

    return expanded

def checksum(data):
    checksum = []
    data_len = len(data)
    for idx in range(0, data_len, 2):
        if idx+1 == data_len or data[idx] != data[idx+1]:
            checksum.append('0')
        else:
            checksum.append('1')
    return ''.join(checksum)

def get_odd_checksum(data):
    check = checksum(data)

    while len(check) % 2 == 0:
        check = checksum(check)

    return check

if __name__ == '__main__':
    expanded_data = expand(INITIAL_DATA, DISK_LENGTH)
    odd_checksum = get_odd_checksum(expanded_data)

    print('The checksum is: {}'.format(odd_checksum))
