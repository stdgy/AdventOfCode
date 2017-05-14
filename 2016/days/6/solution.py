class MessageDecoder(object):

    def record_column_value(self, column, value):
        """
        :type self: MessageDecoder
        :type column: int
        :type value: str
        """
        c_map = self.column_map.get(column, {})
        c_map[value] = c_map.get(value, 0) + 1
        self.column_map[column] = c_map

    def _get_most_frequent_val(self, column):
        """
        :type self: MessageDecoder
        :type column: int
        Returns the most common value stored in this column.
        """
        max_value = 0
        max_key = None
        c_map = self.column_map[column]
        for key in c_map:
            if c_map[key] > max_value:
                max_value = c_map[key]
                max_key = key
        return max_key

    def decode_message(self):
        """
        :type self: MessageDecoder
        :rtype: str
        Returns the decoded message by analyzing the frequency of the letter
        in each column.
        """
        message = []
        keys = list(self.column_map.keys())
        keys.sort()
        for column in keys:
            val = self._get_most_frequent_val(column)
            message.append(val)
        return ''.join(message)

    def __init__(self):
        # Links columns to a count of all of the values store din taht column
        self.column_map = {}

if __name__ == '__main__':
    decoder = MessageDecoder()
    with open('input.txt') as input_file:
        for line in input_file:
            for column, token in enumerate(line):
                decoder.record_column_value(column, token)
    print('{}'.format(decoder.decode_message()))