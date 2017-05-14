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
    password = [None, None, None, None,
                None, None, None, None]

    while None in password:
        token_hash = compute_hash(token, increment)
        if token_hash[:5] == "00000":
            try:
                idx = int(token_hash[5])
                val = token_hash[6]
                if idx < 8 and password[idx] is None:
                    password[idx] = val
                    print('Increment: {}, Password: {}'.format(increment, password))
            except ValueError:
                None
        increment = increment + 1
    
    print('Password is: {}'.format(''.join(password)))