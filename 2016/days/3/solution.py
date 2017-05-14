def is_triangle(a, b, c):
    if a + b <= c:
        return False
    if b + c <= a:
        return False
    if a + c <= b:
        return False
    return True

if __name__ == "__main__":
    valid_triangle_count = 0
    with open('input.txt') as input_file:
        for line in input_file:
            line = line.strip()
            split_line = line.split()
            split_line = filter(lambda x: x != '', split_line)
            a, b, c = map(int, split_line)
            if is_triangle(a, b, c):
                valid_triangle_count = valid_triangle_count + 1
            else:
                print("Invalid: {}, {}, {}".format(a, b, c))
    print("There are {} valid triangles.".format(valid_triangle_count))
