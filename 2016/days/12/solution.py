registers = {
    'a': 0,
    'b': 0,
    'c': 1,
    'd': 0
}

instructions = []

program_counter = 0

def inc(register):
    registers[register] = registers[register] + 1

def dec(register):
    registers[register] = registers[register] - 1

def cpy(from_register, to_register):
    try:
        from_register = int(from_register)
    except ValueError:
        pass # Must not be an int

    if isinstance(from_register, int):
        registers[to_register] = from_register
    else:
        registers[to_register] = registers[from_register]

def jnz(register, jump_amt):
    try:
        register = int(register)
    except ValueError:
        pass # Must not be an int.

    if isinstance(register, int) and register != 0:
        return int(jump_amt)

    if registers[register] != 0:
        return int(jump_amt)
    return 1

def read_instructions(filename):
    with open(filename) as f:
        for line in f:
            instructions.append(line)

def parse_register(instruction):
    """
    :type instruction: str
    """
    p = instruction.strip()
    p = p.split(' ')
    return p[1]

def parse_registers(instruction):
    """
    :type instruction: str
    """
    p = instruction.strip().split(' ')
    return (p[1], p[2])

def execute(instruction, program_counter):
    code = instruction[:3]
    if code == 'cpy':
        register1, register2 = parse_registers(instruction)
        cpy(register1, register2)
        program_counter = program_counter + 1
    elif code == 'inc':
        register = parse_register(instruction)
        inc(register)
        program_counter = program_counter + 1
    elif code == 'dec':
        register = parse_register(instruction)
        dec(register)
        program_counter = program_counter + 1
    elif code == 'jnz':
        register, jump_amt = parse_registers(instruction)
        program_counter = program_counter + jnz(register, jump_amt)

    return program_counter

if __name__ == '__main__':
    # Pull in all of the instructions
    read_instructions('input.txt')

    program_length = len(instructions)
    program_counter = 0

    # Execute the instructions
    while program_counter < program_length:
        program_counter = execute(instructions[program_counter], program_counter)
        if program_counter == 15:
            print('d = {}'.format(registers['d']))

    print('Value of register a is: {}'.format(registers['a']))
