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
- [x] IR formatting
  - [x] Text (like weird assembly)
  - [x] Graph
- [x] IR-based optimizations
  - [x] Copy propagation
  - [x] Common subexpression elimination
- [ ] DLX backend
  - [ ] Register allocation
  - [x] Codegen
- [ ] Tons and tons of tests
  - [ ] Scanner
  - [x] Parser
  - [ ] IR generation
  - [ ] CSE
    - [ ] Domination hierarchy construction
  - [ ] Constant propagation (make sure it _specifically_ works)
  - [ ] IR formatting
    - [ ] Text
    - [ ] Graph
  - [ ] Register allocation
  - [ ] Codegen

### Should Have
- [x] Constant propagation
- [ ] Config levels
  * Just the general idea of providing a way to opt in or out of certain advanced features
  * Mainly useful for offering a "standard" mode which enables exactly the set of features required for CS 142B, and then other mode(s) for advanced stuff
  * Likely manifests itself in 2 ways: optimization levels and "strict mode" (which enables extra semantic checks)

### Nice to Have
- [ ] More optimizations
  - [ ] Dead code elimination
- [ ] ARM/macOS backend & codegen (with or without system `cc` dependency)
- [ ] x86/macOS backend & codegen (with or without system `cc` dependency)
- [ ] x86/Linux backend & codegen (with or without system `cc` dependency)
