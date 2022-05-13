# tinyc
The Tiny compiler

Made for UCI CS 142B; actual readme to come

## Utility Scripts
The `justfile` provides a few utility scripts to make life easier:
* `build`: Invoke the compiler to automatically build a DLX binary for a source file
* `emu`: Run the DLX emulator for a binary (i.e. without having to worry about all the flags)
* `e2e`: Automatically build and run a DLX binary (i.e. automate the whole end-to-end pipeline)

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
- [ ] Register allocation
- [x] DLX backend
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
  - [ ] End to end

### Should Have
- [x] Constant propagation
- [x] Custom DLX simulator
    * Done in Rust
    * May require converting to cargo workspace
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
