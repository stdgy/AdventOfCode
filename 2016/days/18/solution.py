
def is_trap(left, center, right):
    combined = left + center + right
    if combined == '^^.':
        return True
    elif combined == '.^^':
        return True
    elif combined == '^..':
        return True
    elif combined == '..^':
        return True
    return False

def get_next_row(row):
    next_row = []
    padded_row = '.' + row + '.'
    row_len = len(padded_row)
    for idx in range(1, row_len-1):
        left, center, right = padded_row[idx-1], padded_row[idx], padded_row[idx+1]
        if is_trap(left, center, right):
            next_row.append('^')
        else:
            next_row.append('.')
    return ''.join(next_row)

def get_safe_space_count(lines):
    count = 0
    for line in lines:
        count = count + line.count('.')
    return count

if __name__ == '__main__':
    line = ''
    with open('input.txt') as f:
        for l in f:
            line = l
    lines = [line]
    rows = 1
    while rows < 40:
        line = get_next_row(line)
        lines.append(line)
        rows = rows + 1

    
    safe_spaces = get_safe_space_count(lines)

    print('{}'.format('\n'.join(lines)))
    print('Number of safe spaces: {}'.format(safe_spaces))
