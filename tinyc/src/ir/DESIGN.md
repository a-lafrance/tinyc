# Instruction Set
* `mul x, y`: multiplies the two numbers together
* `div x, y`: integer-divides the two numbers together
* `add x, y`: adds the two numbers together
* `sub x, y`: subtracts
* `cmp x, y`: compares the two numbers, producing one of six outcomes: ==, !=, >, >=, <, <=
* a bunch of branch instructions
    * `br dest` (unconditional branch, for jumping to a join block)
    * `beq dest`
    * `bne dest`
    * `bgt dest`
    * `bge dest`
    * `blt dest`
    * `ble dest`
* `read`: read input
* `write x`: write output
* `writeln`: write new line
* `const X`: load constant
* `phi x, y`: phi instruction, like in class
* `end`: terminate the program
    * is this necessary?
* `nop`: no-op instruction, good for placeholders

## ISA Extension for Functions
Function calls are a bit weird because each function is its own separate CFG. This means a couple things:
1. Function calls don't count as control flow, because to the caller the function is a black box that always continues to the next instruction when it returns
2. Note, though, that returning from the callee absolutely does count as control flow, because that's equivalent to the `end` instruction for a custom function body
3. We'll need some way to pass parameters, which doesn't really come easy given the current ISA

Parameters are passed via an imaginary "parameter stack". This is an abstract IR, so the details of that stack's implementation are irrelevant. Point being, all we care about is that the caller pushes parameters onto an imaginary stack reserved for their passage, and the callee will immediately pop them from the stack as soon as its body is entered. This is important because it means they must be passed last-first, so that they can be popped off in order. The choice of stack rather than queue (which, for an abstract machine, might be more elegant) is just so the IR more closely matches how physical machines work, so that it's easier to go from IR to assembly.

The following instructions will be added to the IR ISA to support user-defined functions:
* `ret` and `ret x`: return, with or without a return value
* `call func`: call the given function (assumes parameters have been passed already)
    * question: what about using return values? i say we optionally let this be assigned to a value
* `push val`: push the next parameter onto the parameter stack
* `pop`: pop the top parameter from the parameter stack

# Edge Cases
* Instructions that modify and assign to the same variable: `let a <- a + 1`
