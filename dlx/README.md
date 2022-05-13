# dlx
Rust DLX emulator and ISA implementation

Implements the subset of the DLX architecture required by the compiler, including an emulator and an ISA implementation. The emulator is used in e2e tests for the compiler (see `tinyc/tests`), and the ISA implementation is used during codegen.

The crate exports all its functionality in a library, but provides the `emu` binary for running it directly (see the main README for details).
