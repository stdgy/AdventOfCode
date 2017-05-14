import re

class Output(object):
    def __init__(self):
        self.values = []

class Bot(object):
    def __init__(self):
        self.instructions = []
        self.values = []

def get_bot(bots, bot_id):
    if bots.get(bot_id) is None:
        bots[bot_id] = Bot()
    return bots[bot_id]

def parse_line(line, bots):
    if line[:4] == 'bot ':
        found = re.findall('(\w+)\s(\d+)', line)
        bot_id = int(found[0][1])
        low_type = found[1][0]
        low_id = int(found[1][1])
        high_type = found[2][0]
        high_id = int(found[2][1])
        instruction = {'low': low_id, 'low_type': low_type, 'high': high_id, 'high_type': high_type}

        bot = get_bot(bots, bot_id)

        bot.instructions.insert(0, instruction)

    elif line[:5] == 'value':
        found = re.findall('\d+', line)
        found = list(map(int, found))
        value_id = found[0]
        bot_id = found[1]

        bot = get_bot(bots, bot_id)

        bot.values.append(value_id)

def add_to_output(outputs, output_id, value):
    if outputs.get(output_id) is None:
        output = Output()
        outputs[output_id] = output
    outputs[output_id].values.append(value)

def execute(bots, outputs):
    # Find bot with 2 chips and execute an instruction
    for bot_id in bots:
        bot = bots[bot_id]
        if len(bot.values) == 2:
            low_val = min(bot.values)
            high_val = max(bot.values)
            bot.values = []
            instruction = bot.instructions.pop()

            if instruction['low_type'] == 'bot':
                bots[instruction['low']].values.append(low_val)
            else:
                add_to_output(outputs, instruction['low'], low_val)

            if instruction['high_type'] == 'bot':
                bots[instruction['high']].values.append(high_val)
            else:
                add_to_output(outputs, instruction['high'], high_val)
            
            if low_val is 17 and high_val is 61:
                print('Bot {} is responsible.'.format(bot_id))
            return True
    return False

if __name__ == '__main__':
    bots = {}
    outputs = {}
    # Read input file and create bots
    with open('input.txt') as file:
        for line in file:
            parse_line(line, bots)

    # Begin bot execution
    continueToExecute = execute(bots, outputs)

    while continueToExecute:
        continueToExecute = execute(bots, outputs)

    print('Multiplication of output 1, 2 and 3: {}'.format(outputs[0].values[0] * outputs[1].values[0] * outputs[2].values[0]))
