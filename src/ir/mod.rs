// instruction set
// mul x, y: multiplies the two numbers together
// idiv x, y: integer-divides the two numbers together
// add x, y: adds the two numbers together
// sub x, y: subtracts
// cmp x, y: compares the two numbers, producing one of six outcomes: ==, !=, >, >=, <, <=
// a bunch of branch instructions
    // beq dest
    // bne dest
    // bgt dest
    // bge dest
    // blt dest
    // ble dest
// ret [x]: return, optionally with value
// call func(x, y): call function with given params
// ldparam: "loads" the next parameter
    // this is just there to assign it a value in the function's basic block
// read: read input
// write x: write output
// writeln: write new line
// const X: load constant
