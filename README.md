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

### Requirements
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
- [x] Register allocation
- [x] DLX backend
  - [x] Codegen
- [ ] Tons and tons of tests
  - [x] Scanner
  - [x] Parser
  - [x] AST visitor
  - [ ] IR generation
  - [ ] Domination hierarchy construction
  - [ ] CSE
  - [ ] Constant propagation
  - [ ] Instruction selection
  - [ ] Dead code elimination
  - [ ] IR formatting
    - [ ] Text
    - [ ] Graph
  - [ ] IR visitor
  - [ ] Register allocation
  - [ ] Codegen
  - [ ] End to end
    - [ ] Full pipeline tests (build and run DLX binary)
    - [ ] IR generation tests (verify IR for actual programs)

### Stretch Goals
- [x] Constant propagation
- [x] Custom DLX simulator
    * Done in Rust
    * May require converting to cargo workspace
- [ ] More optimizations
  - [x] Dead code elimination
  - [x] Basic (very basic) instruction selection
    * Meaning basically just prefer immediate instructions when exactly one operand is a constant
    * IMPORTANT: you may reorder operands to achieve this ONLY IF the operation is commutative
  - [ ] "Dead constant elimination"
    * Basically just stop allocating values for unused constants by detecting and eliminating them during IR generation
- [x] Config levels
  * Just the general idea of providing a way to opt in or out of certain advanced features
  * Mainly useful for offering a "standard" mode which enables exactly the set of features required for CS 142B, and then other mode(s) for advanced stuff
  * Likely manifests itself in 2 ways: optimization levels and "strict mode" (which enables extra semantic checks)

### Extreme Stretch Goals
- [ ] Dump assembly during codegen
    * Change `--dump-ir` to just `--dump` with 3 options:
        * `ir` dumps IR as text
        * `cfg` (or `ir-cfg`) dumps IR as graph
        * `asm` dumps assembly as text (_must_ appear with `--arch` for codegen)
- [ ] Even more optimizations
  - [x] Compile time evaluation of constant comparisons/branching
- [ ] Migrate to `flexstr` wherever possible because it'll likely be much more efficient
- [ ] Additional language features
    - [ ] Logical operators (and/or/not)
    - [ ] Bit shift operators
    - [ ] Bitwise operators
    - [ ] Break/continue for loops
    - [ ] else-if branches for if statements
- [ ] Complete backend for a real target (probably either ARM/macOS, x86/Linux, or ARM/Linux)
- [ ] LLVM IR backend + actual LLVM integration (this one's pretty extreme lol)
