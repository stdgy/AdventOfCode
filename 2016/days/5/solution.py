import hashlib

def compute_hash(prefix, increment):
    """
    :type prefix: str
    :type increment: int
    """
    bs = bytes(prefix + str(increment), 'utf-8')
    md5_hash = hashlib.new('md5')
    md5_hash.update(bs)
    return md5_hash.hexdigest()


if __name__ == '__main__':
    token = 'wtnhxymk'
    increment = 0
    password = []

    while len(password) < 8:
        token_hash = compute_hash(token, increment)
        if token_hash[:5] == "00000":
            password.append(token_hash[5])
            print(password)
        increment = increment + 1
