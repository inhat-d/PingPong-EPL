# PingPong-EPL

## Introduction

This repository contains an interpreter for a custom stack-based (esoteric) programming language. The interpreter processes a simple assembly-like language with commands to perform arithmetic operations, manage a stack, handle conditional jumps, and print messages. It is implemented in Python and provides a foundation for experimenting with stack-based virtual machines.

### Key Features:
- **Stack Operations**: Push, pop, and perform arithmetic operations directly on a stack.
- **Conditional Jumps**: Execute control flow logic based on stack values.
- **Custom Labels**: Use labels for structured jumps within the program.
- **Simple Syntax**: An easy-to-learn language format designed for learning and experimentation.

## Usage

### Prerequisites
Ensure you have Python 3 installed on your system.

### Writing a Program
Create a text file containing your program code. For example:

```
READ
READ
ADD
PING
STOP
```

### Running the Interpreter
1. Clone the repository:
   ```bash
   git clone https://github.com/inhat-d/PingPong-EPL
   cd PingPong-EPL
   ```

2. Execute the interpreter with your program file:
   ```bash
   python interpreter.py <path_to_your_program>
   ```

### Supported Instructions
| **Instruction**  | **Description**                                                                 |
|-------------------|---------------------------------------------------------------------------------|
| `PING`            | Prints top stack's number.                                                    |
| `PONG`            | Pushes a number onto the stack.                                               |
| `DELETE`          | Removes the top value from the stack.                                         |
| `ADD`             | Pops two numbers from the stack, adds them, and pushes the result.            |
| `SUB`             | Pops two numbers from the stack, subtracts them, and pushes the result.       |
| `PRINT`           | Prints some text.                                                             |
| `READ`            | Reads a number from the user and pushes it onto the stack.                    |
| `JUMP.EQ.0 <label>`| Jumps to a label if the top value on the stack is 0.                         |
| `JUMP.GT.0 <label>`| Jumps to a label if the top value on the stack is greater than 0.            |
| `STOP`            | Stops program execution.                                                      |

### Example Program
```assembly
READ
READ
SUB
JUMP.EQ.0 L1
PRINT "not equal"
STOP

L1:
PRINT "equal"
STOP
```

```
I: 3
I: 2
O: not equal
```

### Output for the Example
```
Not Zero
Done
