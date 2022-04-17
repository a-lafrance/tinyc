# tinyc
The Tiny compiler

Made for UCI CS 142B; actual readme to come

## Roadmap
Roadmap for goals to accomplish this quarter:

### Must Have
- [x] Basic frontend (scanner, parser, AST)
- [x] IR instruction set design
- [x] IR generation from AST
  - [x] Support for user-defined functions
  - [ ] Refactor to the "instruction pointer" IR model
- [x] IR formatting
  - [x] Text (like weird assembly)
  - [x] Graph
- [ ] IR-based optimizations
  - [x] Copy propagation
  - [ ] Common subexpression elimination

### Should Have
- [ ] DLX backend & codegen
- [ ] More optimizations
  - [ ] Constant propagation
  - [ ] Dead code elimination
- [ ] Optimization levels
  * Just the general idea of providing a way to opt in or out of certain advanced features
  * Mainly useful for offering a "standard" mode which enables exactly the set of features required for CS 142B, and then other mode(s) for advanced stuff

### Nice to Have
- [ ] ARM/macOS backend & codegen (with or without system `cc` dependency)
- [ ] x86/macOS backend & codegen (with or without system `cc` dependency)
- [ ] x86/Linux backend & codegen (with or without system `cc` dependency)
