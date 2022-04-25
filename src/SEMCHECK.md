# Semantic Checks Brainstorm
Just some basic semantic checks that are worth performing

- [x] Identifier (var/func name) can't be reserved word
    * Handled by tokenization
- [x] Only non-void functions in expressions, only void functions in statements
- [x] Undefined variables and functions can't be referenced
    * Builtin functions come pre-populated
- [x] Function w/ same name can't be defined twice
- [ ] Function is called w/ required number of arguments
- [ ] Warn when using uninitialized variable
    * In terms of implementation, this refers to using a variable that doesn't have a value already
    * Don't forget to assign an initial value of 0, but this only matters when generating native code
