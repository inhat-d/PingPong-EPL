# Custom Stack-Based Language Interpreter

## Introduction

This repository contains an interpreter for a custom stack-based programming language. The interpreter processes a simple assembly-like language with commands to perform arithmetic operations, manage a stack, handle conditional jumps, and print messages. It is implemented in Python and provides a foundation for experimenting with stack-based virtual machines.

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
PUSH 10
PUSH 20
ADD
PRINT "Result:"
POP
STOP
```

### Running the Interpreter
1. Clone the repository:
   ```bash
   git clone <repository_url>
   cd <repository_folder>
   ```

2. Execute the interpreter with your program file:
   ```bash
   python interpreter.py <path_to_your_program>
   ```

### Supported Instructions
| **Instruction**  | **Description**                                                                 |
|-------------------|---------------------------------------------------------------------------------|
| `PUSH <number>`   | Pushes a number onto the stack.                                                |
| `POP`             | Removes the top value from the stack.                                         |
| `ADD`             | Pops two numbers from the stack, adds them, and pushes the result.            |
| `SUB`             | Pops two numbers from the stack, subtracts them, and pushes the result.       |
| `PRINT <string>`  | Prints a string literal (enclosed in double quotes).                          |
| `READ`            | Reads a number from the user and pushes it onto the stack.                   |
| `JUMP.EQ.0 <label>`| Jumps to a label if the top value on the stack is 0.                         |
| `JUMP.GT.0 <label>`| Jumps to a label if the top value on the stack is greater than 0.            |
| `<label>:`        | Defines a label for jump instructions.                                        |
| `STOP`            | Stops program execution.                                                     |

### Example Program
```assembly
PUSH 10
PUSH 5
SUB
JUMP.EQ.0 end
PRINT "Not Zero"
end:
PRINT "Done"
STOP
```

### Output for the Example
```
Not Zero
Done
