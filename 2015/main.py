from days.five import Five

f = Five(input_file='inputs/5.txt')
count = f.get_nice_word_count()

print('Number of nice words: {}'.format(count))