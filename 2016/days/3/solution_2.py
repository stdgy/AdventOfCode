def is_triangle(a, b, c):
    if a + b <= c:
        return False
    if b + c <= a:
        return False
    if a + c <= b:
        return False
    return True

def parse_numbers_from_line(line):
    line = line.strip()
    split_line = line.split()
    split_line = filter(lambda x: x != '', split_line)
    a, b, c = map(int, split_line)
    return (a, b, c)

if __name__ == "__main__":
    valid_triangle_count = 0
    with open('input.txt') as input_file:
        while input_file.readable():
            line_1 = input_file.readline()
            if line_1 == '':
                break
            line_2 = input_file.readline()
            line_3 = input_file.readline()

            print('line_1: {}'.format(line_1))

            line_1 = parse_numbers_from_line(line_1)
            line_2 = parse_numbers_from_line(line_2)
            line_3 = parse_numbers_from_line(line_3)

            for i in range(3):
                a, b, c = line_1[i], line_2[i], line_3[i]
                if is_triangle(a, b, c):
                    valid_triangle_count = valid_triangle_count + 1

    print("There are {} valid triangles.".format(valid_triangle_count))
