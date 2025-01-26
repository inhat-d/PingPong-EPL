# Created 18.01.2025
# By @tiny_mauss
# Or @inhat-d on GitHub
# Usage: `python3 interpreter.py [file]`

# PingPong-EPL 

import sys

program_filepath = sys.argv[1]  #makes this thing: `python3 interpreter.py [file]`

program_lines = []
with open(program_filepath, "r") as program_file:   #opens the file
    program_lines = [line.strip() for line in program_file.readlines()]

program = []        #defines
token_counter = 0   #the
label_tracker = {}  #variables
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    if opcode == "":
        continue

    if opcode.endswith(":"):  #labels
        label_tracker[opcode[:-1]] = token_counter
        token_counter += 1
        continue

    program.append(opcode)  #commands
    token_counter += 1

    #making commands add/move stack
    if opcode == "PONG":
        number = int(parts[1])
        program.append(number)
        token_counter += 1
    elif opcode == "PRINT":
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
        token_counter += 1
    elif opcode == "JUMP.EQ.0" or opcode == "JUMP.GT.0":
        label = parts[1]
        program.append(label)
        token_counter += 1

class Stack:

    def __init__(self, size):
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push(self, number):
        self.sp += 1
        self.buf[self.sp] = number

    def pop(self):
        number = self.buf[self.sp]
        self.sp -= 1
        return number
    
    def top(self):
        return self.buf[self.sp]

pc = 0
stack = Stack(256)

while program[pc] != "STOP":    #"STOP" - Well... Stops the program
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":    #adds new number to stack
        number = program[pc]
        pc += 1
        stack.push(number)
    elif opcode == "DELETE":    #deletes top number
        stack.pop()
    elif opcode == "ADD":   #adds top two numbers and pushes to top
        a = stack.pop()
        b = stack.pop()
        stack.push(a + b)
    elif opcode == "SUB":   #subtracts top two numbers and pushes to top
        b = stack.pop()
        a = stack.pop()
        stack.push(a - b)
    elif opcode == "PING": #prints top number
        print(stack.top())
    elif opcode == "PRINT": #prints some text
        print(program[pc])
        pc += 1
    elif opcode == "READ": #reads a number
        number = float(input())
        stack.push(number)
    elif opcode == "JUMP.EQ.0": #if top number is 0, jumps to label
        number = stack.pop()
        if number == 0:
            pc = label_tracker[program[pc]]
        else:
            pc += 1
    elif opcode == "JUMP.GT.0": #if top number is greater than 0, jumps to label
        number = stack.pop()
        if number > 0:
            pc = label_tracker[program[pc]]
        else:
            pc += 1