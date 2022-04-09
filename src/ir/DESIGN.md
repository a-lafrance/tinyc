# Instruction Set
* `mul x, y`: multiplies the two numbers together
* `div x, y`: integer-divides the two numbers together
* `add x, y`: adds the two numbers together
* `sub x, y`: subtracts
* `cmp x, y`: compares the two numbers, producing one of six outcomes: ==, !=, >, >=, <, <=
* a bunch of branch instructions
    * `b dest` (unconditional branch, for jumping to a join block)
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

Instructions to handle function calls, required for step 3:
* `ret [x]`: return, optionally with value
* `call func(x, y)`: call function with given params
* `ldparam`: "loads" the next parameter
    * this is just there to assign it a value in the function's basic block

# Implementation Design
* next question: in the compiler, how is IR actually implemented?
* where are instructions & basic blocks stored? where is the value table? how is that implemented?
* how is the ast lowered into ir?

* both instructions and basic blocks are interned
* somewhere, there will be a giant buffer of instructions
* there will also be a giant buffer of basic blocks
* instructions will carry info about their parameters
* basic blocks will have:
    * a list of instructions inside it
    * a fallthrough basic block, if it exists
        * must be optional because the end block has no fallthrough, it terminates the program
    * a branch destination basic block, if it exists
        * must be optional for the same reason as above (this also helps the prelude block)
* there will be a special "prelude" basic block used for allocating constants
    * this block will fallthrough into the "real" first block, ie the first non-prelude block
