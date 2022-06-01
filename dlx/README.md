# dlx
Rust DLX emulator and ISA implementation, written by: Arthur Lafrance (Spring 2022)

## Overview

This is a Rust implementation of the DLX ISA. Included are a library implementation of the ISA and two binaries: `emu`, a DLX emulator; and `viz`, a DLX instruction printer. The library is intended to be used for implementing a DLX compiler backend, and the binaries are meant for testing and debugging.

## Usage

The best way to use the ISA library is to add it as a local path dependency of your Cargo project, since I never had much interest in putting it on `crates.io`.

The best way to use the binaries is either to add them to your Cargo project and call them with `cargo run`, or to install them directly by running `cargo install --bin <BIN> --features cli --path /path/to/dlx`, where `BIN` is the name of the binary you want to install and `/path/to/dlx` is the path to the root directory of this project. The `cli` feature is required because I made it an optional feature (which is required by the binaries, obviously), so the library crate doesn't get bogged down by the extra dependency. Once installed, you can invoke them directly, like `emu a.out` or `viz a.out`.

## Notes

**Importantly**, this crate only implements the subset of the DLX ISA that I needed for my compiler, so if you end up doing something too far out of the scope of the requirements, you may run into issues with unsupported instructions, in which case you're welcome to add that functionality yourself.

And as one final note for posterity, the original source code for this crate can be found [here](https://github.com/a-lafrance/tinyc/tree/master/dlx) (although please don't steal the actual compiler code for academic dishonesty).
