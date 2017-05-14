
MIN_ADDRESS = 0
MAX_ADDRESS = 4294967295

def parse_filter(line):
    start, end = map(int, line.split('-'))
    return (start, end)

def is_allowed(filters, address):
    for min_range, max_range in filters:
        if address >= min_range and address <= max_range:
            return False
        if min_range > address:
            return True
    return True

def find_minimum(filters):
    for address in range(MIN_ADDRESS, MAX_ADDRESS):
        if is_allowed(filters, address):
            return address
    return None

if __name__ == '__main__':
    filters = []
    with open('input.txt') as filter_file:
        for line in filter_file:
            filters.append(parse_filter(line))

    # Order filters 
    filters.sort()

    minimum = find_minimum(filters)

    print('Minimum address: {}'.format(minimum))
