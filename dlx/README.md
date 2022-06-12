# dlx
Rust DLX emulator and ISA implementation

This crate implements the entire DLX architecture as described in the spec provided in CS 142B. An ISA implementation is provided as a library crate, and two binaries are provided: a DLX emulator and a DLX assembly printer.

To use the library crate, I recommend specifying it as a [git dependency](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#specifying-dependencies-from-git-repositories) with Cargo:

```toml
[dependencies]
dlx = { git = "https://github.com/a-lafrance/tinyc.git" }
# ...
```

To use the binaries, I recommend installing them with Cargo. `emu` is the emulator, and `viz` is the assembly visualizer (it takes a DLX binary and prints it out as assembly): `cargo install --git https://github.com/a-lafrance/tinyc.git --features cli --bin <BIN> -- dlx`, where `BIN` is either `emu` or `viz`. Then, the binary will be executable by name in your `PATH`.
