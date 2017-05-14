
MIN_ADDRESS = 0
MAX_ADDRESS = 4294967295

def parse_filter(line):
    start, end = map(int, line.split('-'))
    return (start, end)

def find_num_allowed_ips(filters):
    allowed = 0
    last_start = 0
    last_end = 0
    for filter_start, filter_end in filters:
        if filter_start > (last_end + 1):
            allowed = allowed + (filter_start - (last_end + 1))
        last_start = filter_start
        last_end = filter_end

    allowed = allowed + (MAX_ADDRESS - last_end)
    return allowed

def print_filters(filters):
    for filter in filters:
        print(filter)

def combine_groups(filters):
    combined_filters = []
    idx = 0
    length = len(filters)
    skipped = False
    while idx < length:
        filter = filters[idx]
        start, end = filter
        for idx2 in range(idx+1, len(filters)):
            start2, end2 = filters[idx2]

            # Expand the range
            if start2 < end and end2 > end:
                end = end2

            # Discontinuity
            if start2 > end:
                idx = idx2
                skipped = True
                break

            if idx2 == length - 1:
                idx = length

        combined_filters.append((start, end))

        if skipped:
            skipped = False
        else:
            idx = idx + 1
    return combined_filters


if __name__ == '__main__':
    filters = []
    with open('input.txt') as filter_file:
        for line in filter_file:
            filters.append(parse_filter(line))

    filters.sort()
    filters = combine_groups(filters)

    num_ips_allowed = find_num_allowed_ips(filters)

    print('Number of allowed addresses: {}'.format(num_ips_allowed))
