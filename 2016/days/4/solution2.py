import re

def parse_name(input):
    """
    :type input: str
    Returns a map of the room name, sector name and checksum.
    """
    r = re.search('^([a-zA-Z-]+)(\d+)\[(.*)\]', input)

    room_name = r.group(1)
    room_name = room_name.replace('-', '')

    sector_name = int(r.group(2))

    checksum = r.group(3)

    return {
        'room_name': room_name,
        'sector_name': sector_name,
        'checksum': checksum
    }

def calculate_letter_frequency(word):
    """
    :type word: str
    Returns a map containing the number of times each letter is used.
    """
    freq = {}
    for letter in word:
        freq[letter] = freq.get(letter, 0) + 1
    return freq

def sort_frequencies(freqs):
    """
    :type freqs: dict
    :rtype: list
    Returns a list of keys sorted by value
    """
    letters = list(freqs.keys())
    letters.sort()
    combined_vals = []
    for letter in letters:
        combined_vals.append({
            'key': letter,
            'val': freqs[letter]
        })

    combined_vals.sort(key=lambda v: v.get('val'), reverse=True)
    sorted_keys = list(map(lambda v: v.get('key'), combined_vals))
    return sorted_keys

def decrypt_room_name(room_name, rounds):
    """
    :type room_name: str
    :type rounds: int
    :rtype: str
    Returns a decrypted room name.
    """
    a = ord('a')
    z = ord('z')
    alph_count = z - a + 1
    decrypted_name = ""

    for letter in room_name:
        l = ord(letter) - a 
        l = (l + rounds) % alph_count 
        l = l + a 
        decrypted_name = decrypted_name + chr(l)

    return decrypted_name

if __name__ == '__main__':
    total = 0
    with open('input.txt') as file:
        for line in file:
            # Parse the line
            vals = parse_name(line)
            # Calculate the letter frequencies
            frequencies = calculate_letter_frequency(vals['room_name'])
            # Sort the frequencies
            frequencies = sort_frequencies(frequencies)
            # If the sorted frequencies match the checksum, add sector id to
            # total
            list_check = list(vals['checksum'])
            if list_check == frequencies[:len(list_check)]:
                total = total + vals['sector_name']
                decrypted_name = decrypt_room_name(vals['room_name'], vals['sector_name'])

                if 'north' in decrypted_name:
                    print('Name: {} Sector Name: {}'.format(decrypted_name, vals['sector_name']))
    print('The sum of all sector names is: {}'.format(total))
