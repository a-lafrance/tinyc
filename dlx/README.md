# dlx
Rust DLX emulator and ISA implementation

## Overview

This crate implements the entire DLX architecture as described in the spec provided in CS 142B. An ISA implementation and emulator are provided as a library crate, and two binaries are provided: a runner for the emulator and a DLX assembly printer.

To use the library crate, I recommend specifying it as a [git dependency](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#specifying-dependencies-from-git-repositories) with Cargo:

```toml
[dependencies]
dlx = { git = "https://github.com/a-lafrance/tinyc.git" }
# ...
```

To use the binaries, I recommend installing them with Cargo. `emu` is the emulator, and `viz` is the assembly visualizer (it takes a DLX binary and prints it out as assembly): `cargo install --git https://github.com/a-lafrance/tinyc.git --features cli --bin <BIN> -- dlx`, where `BIN` is either `emu` or `viz`. Then, the binary will be executable by name in your `PATH`.

You may also use the crate from source directly, in which case you'll probably want to use [Cargo workspaces](https://doc.rust-lang.org/book/ch14-03-cargo-workspaces.html) and [path dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#specifying-path-dependencies).

## Use Cases

I used the ISA implementation to write code generation for my DLX backend. It comes with functionality for serializing instructions to/from raw bytes for that purpose.

I used the binaries to manually test my compiler, so that I could inspect and run the code that was generated to make sure it was correct.

The emulator was also really useful for automated tests; because it's exported with the library crate, I could invoke it directly in my native Cargo tests.
