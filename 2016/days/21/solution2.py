import re

INITIAL_TEXT = 'fbgdceah'

def swap_position(text, pos_x, pos_y):
    ltext = list(text)
    temp = ltext[pos_x]
    ltext[pos_x] = ltext[pos_y]
    ltext[pos_y] = temp
    return ''.join(ltext)

def swap_letter(text, letter_x, letter_y):
    index_x = text.index(letter_x)
    index_y = text.index(letter_y)
    return swap_position(text, index_x, index_y)

def rotate(text, direction, amount):
    rotated = ''
    if direction == 'right':
        index = amount % len(text)
    else:
        index = -amount % len(text)

    for char in text:
        rotated = rotated + text[index]
        index = (index + 1) % len(text)

    return rotated

def rotate_from_letter(text, letter):
    index = text.index(letter)
    reverse_map = [9, 1, 6, 2, 7, 3, 8, 4]
    rotation = reverse_map[index] # Pre-calculated the reverse map
    return rotate(text, 'right', rotation)

def reverse(text, pos_x, pos_y):
    reversed = []
    idx = 0
    while idx < pos_x:
        reversed.append(text[idx])
        idx = idx + 1
    idx = pos_y
    while idx >= pos_x:
        reversed.append(text[idx])
        idx = idx - 1
    idx = pos_y + 1
    while idx < len(text):
        reversed.append(text[idx])
        idx = idx + 1
    return ''.join(reversed)

def move(text, pos_x, pos_y):
    char = text[pos_x]
    text = text[0:pos_x] + text[pos_x + 1:]
    return text[:pos_y] + char + text[pos_y:]

def execute(text, line):
    if line[0:13] == 'swap position':
        x, y = map(int, re.findall('\d+', line))
        text = swap_position(text, x, y)
    elif line[0:11] == 'swap letter':
        x, y = re.findall('swap letter (\w+) with letter (\w+)', line)[0]
        text = swap_letter(text, x, y)
    elif line[0:12] == 'rotate based':
        letter = re.findall('rotate based on position of letter (\w+)', line)[0]
        text = rotate_from_letter(text, letter)
    elif line[0:6] == 'rotate':
        direction, amount = re.findall('rotate (\w+) (\d+) step', line)[0]
        text = rotate(text, direction, int(amount))
    elif line[0:17] == 'reverse positions':
        x, y = map(int, re.findall('\d+', line))
        text = reverse(text, x, y)
    elif line[0:13] == 'move position':
        x, y = map(int, re.findall('\d+', line))
        text = move(text, y, x)

    return text

if __name__ == '__main__':
    text = INITIAL_TEXT
    lines = []
    with open('input.txt') as file:
        for line in file:
            lines.append(line)
    idx = len(lines) - 1
    print(INITIAL_TEXT)
    while idx > -1:
        text = execute(text, lines[idx])
        idx = idx - 1
    print('Output is: {}'.format(text))
