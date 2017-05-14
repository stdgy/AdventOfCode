import hashlib

def get_hash(salt, index):
    md5 = hashlib.md5()
    message = (salt + str(index)).encode('utf-8')
    md5.update(message)
    hex = md5.hexdigest()

    for index in range(2016):
        md5 = hashlib.md5()
        md5.update(hex.encode('utf-8'))
        hex = md5.hexdigest()

    return md5.hexdigest()

def get_triplet(key):
    for idx in range(len(key)-3):
        if key[idx] == key[idx+1] == key[idx+2]:
            return key[idx]
    return None

def has_five_in_row(key, char):
    for idx in range(len(key)-5):
        if key[idx] == char and key[idx] == key[idx+1] and\
        key[idx+1] == key[idx+2] and key[idx+2] == key[idx+3] and\
        key[idx+3] == key[idx+4]:
            return True
    return False

def check_next_1000_keys(salt, index, char):
    for idx in range(index+1, index+1000):
        hash = get_hash(salt, idx)
        if has_five_in_row(hash, char):
            print('5. char: {}, hash: {}'.format(char, hash))
            return True
    return False

def is_key(salt, key, index):
    triplet = get_triplet(key)

    if triplet is not None:
        if check_next_1000_keys(salt, index, triplet):
            print('3. char: {}, hash: {}'.format(triplet, key))
            return True
    return False

if __name__ == '__main__':
    puzzle_input = 'cuanljph'
    otp_keys = []
    index = 0

    while len(otp_keys) < 64:
        key = get_hash(puzzle_input, index)

        if is_key(puzzle_input, key, index):
            otp_keys.append(key)
            print('Index for {}th key is {}'.format(len(otp_keys), index))
        index = index + 1

    print(otp_keys)
