# tinyc
The Tiny compiler

## Overview

`tinyc` is a compiler for the toy language Tiny, created for UCI CS 142B (Spring 2022). I'd like to give a big thanks to Prof. Franz for offering the class this quarter; I really enjoyed it, and had way too much fun adding extra bells and whistles to this project.

For context, the compiler produces DLX binaries (the toy architecture provided to us) out of Tiny source files. I haven't included any specs for either the Tiny language of the DLX architecture because it's reasonably easy to get the gist just by poking around in the code, if you're so inclined.

## Usage

### Invoking the Compiler

To preface the rest of this section, there are a couple ways to invoke the compiler:
1. With Cargo directly: `cargo run -p tinyc -- <FLAGS> <SRC_FILE>`
    * This isn't the preferred way of doing things because it's kind of annoying to have to type out that long command every time, but it's reasonable.
2. With the provided Just recipes:
    1. `just build <SRC_FILE> <FLAGS>` just invokes the compiler transparently
    2. `just e2e <SRC_FILE> <BIN> <FLAGS>` compiles `SRC_FILE` with `FLAGS` into a DLX binary named `BIN` and executes it with the provided DLX emulator.
3. With a Cargo binary installation, if you want it in your `$PATH`: `cargo install --path tinyc`
    * You can always uninstall it with `cargo uninstall -p tinyc`

### Compiler Functions

So what does the compiler do anyway?
1. Compiles binaries, when you pass it the `--arch <ARCH>` flag. Currently DLX is the only supported architecture, so really you can only pass it `--arch dlx`.
2. Dump the IR that's generated during compilation and end the compilation process there, with the `--dump-ir <FMT>` flag.
    * There are two format options available:
        1. `--dump-ir text` formats IR as a pseudo-assembly text file (see the `.ir` files in `tinyc/tests` for examples).
        2. `--dump-ir graph` formats the IR control-flow graph with the Dot graph description language, which you can then visualize separately however you like.
    * Importantly, compilation **ends** at IR generation with this flag; you can't both dump IR and compile to a binary (which sucks, but is a shortcoming of the current compiler).

### DLX Functions

In addition to the compiler, I also wrote a DLX ISA implementation and small toolchain in Rust (that's the `dlx` crate). Once you have a binary, you can do two things with it:
1. Run it with the provided emulator, `emu`.
2. Print it as assembly with the `viz` binary.

In either case, you have three ways to invoke these binaries (same as invoking the compiler above):
1. With Cargo directly: `cargo run -p dlx --features cli --bin <emu | viz> -- <FLAGS>`
    * This method is explicitly _not_ recommended, because it can be a bit confusing what all of those flags mean, and it's annoying to have to remember and type them out each time. Use either of the bottom two methods instead.
2. With the provided Just recipes: `just emu <BIN> <FLAGS>` or `just viz <BIN> <FLAGS>`
    * This gets all those flags out of the way, so it's much easier to invoke the binaries directly.
3. With a Cargo binary installation: `cargo install --path dlx --features cli --bin <emu | viz>`
    * To uninstall an individual binary, run `cargo uninstall dlx --bin <emu | viz>`

### Running Lints & Tests

To run Clippy lints, use the provided Just recipe: `just check`, which just invokes Clippy with a few flags configured. To run unit and end-to-end tests, run `cargo test` as usual. For more info on what exactly was tested and how, see "Details" below.

## Details

For those curious, here's a bit more detail about what this project actually accomplishes.

### Requirements

Obviously the first thing I did was implement the requirements for the class:
* Frontend
    * Scanner
    * Parser (top-down, recursive descent)
    * AST (well, technically the AST wasn't necessary, but I wrote one anyway)
* Middle-end
    * SSA-based IR instruction set implementation
    * IR generation from the AST
    * IR formatting
        * Text (as pseudo-assembly)
        * Graph (as Dot graphs)
    * Domination hierarchy construction (useful when optimizing, mainly for CSE)
    * Basic optimizations (both performed during IR generation)
        * Copy propagation
        * Common subexpression elimination
* Backend
    * Graph coloring-based register allocation
        * I also wrote a simple naive register allocation before I finished the robust one
    * DLX code generation
        * This was more of a stretch goal rather than a requirement, but I'll include it with the requirements anyway
* Tests (for everything)
    * I also set up very basic CI that runs all tests and lints the project with Clippy on every pull request, just to make my life easier.
* User-defined functions, as a language feature, which mostly requires extending the middle- and backends

### Extras

In addition to the basics, I also added some extra bells and whistles to the project:
* More optimizations
    * Constant propagation, in two parts:
        * For individual instructions with two constant operands that could be folded, I computed their result at compile time and folded the instruction into that result.
        * In order to propagate these constant results as much as possible, I ran multiple passes of constant propagation until there was no more propagation possible.
    * "Sparse conditional constant propagation", i.e. "constant propagation for conditions"
        * For this I tried to compute comparisons at compile-time in the same way I performed constant folding
        * This also allowed me to evaluate branches at compile time if the value being branched on was a constant
        * In total, this was a cool hybrid of constant propagation and dead code elimination; see the example [here](https://en.wikipedia.org/wiki/Constant_folding#The_optimizations_in_action) for a good demo of how it can work.
        * One small thing that bugged me here is I felt like I was tailoring my implementation too closely to DLX-specific details with how I evaluate comparisons at compile time, but whatever.
    * Dead code elimination
        * In terms of dead values, if a value was never used, I never allocated space for it during register allocation (although it was still present in the IR itself).
        * Dead instructions were eliminated during IR generation. Basically, don't generate code after you're sure you've returned.
    * Basic (very basic) instruction selection
        * This just manifested itself as selecting immediate instructions during code generation if one operand was a constant.
    * Constant propagation of both varieties and some dead code elimination was accomplished during IR generation
    * Instruction selection and some dead code elimination was accomplished during register allocation and code generation.
* A system for opting in and out of optimizations
    * I provided three optimization levels, to allow opting into optimizations in bulk:
        1. Bare: no optimizations at all, including naive register allocation
        2. Default: exactly the set of optimizations required for the class (CSE + robust register allocation)
        3. Full: the full suite of optimizations offered by the compiler
    * There are also flags for enabling and disabling each optimization individually
* A full DLX implementation
    * I needed an ISA implementation to write DLX code generation anyway.
    * I also needed a way to test the code that was generated, so I wrote an emulator for the DLX architecture in Rust.
    * Separate to this project, I also wrote the [`discrim`](https://github.com/a-lafrance/discrim) crate specifically for generating what would otherwise have been a giant match statement to initialize opcodes from their integer representation.

### Tests

Finally, here's how I implemented those tests I keep talking about.

The first kind of tests I wrote were standard Rust unit tests. Wherever I had a chunk of functionality that needed to be tested in isolation, I wrote a set of unit tests that abstracted away every other part of the pipeline and focused on just that step.

End-to-end (e2e) tests were much more interesting to set up. I wanted to mimic how Rust's compiler diagnostics are tested, which I really liked, without having to write [`trybuild`](https://github.com/dtolnay/trybuild) myself. What I ended up with is:
* A single function that takes in a test name and runs it (with the source file at `<TEST>.tiny`).
    * If the presence of a `<TEST>.out` file is detected, the source file is compiled into a DLX binary and emulated, and the output is compared against that `.out` file. During emulation, if a `<TEST>.in` file is present, it's used as the stdin to the emulated binary.
    * If a `<TEST>.ir` file is detected, the source file is compiled into its pseudo-assembly IR representation and compared against that file.
    * This is where it sucks that you can't both compile into a binary and dump IR in one run, since I needed to compile the file twice for tests that had both `.out` and `.ir` files.
    * Well, technically I lied a little bit: all the paths listed above are actually in the `tinyc/tests/rsrc/` directory.
* A macro that generates this function call given the parameters of the test to run, since I felt like writing a macro just to condense a few lines of copy-pasted code. Now all I have to write to define a new e2e test is `define_e2e_test!(test_name)`.
* Obviously, I also had to write all the source, input, output, and IR files for the tests.

What I was missing throughout this process was a good crate for diffing files in tests. File diffing crates exist, but in my initial canvas of the landscape none of them provided exactly what I was looking for: a nice-looking diff that shows up when a comparison fails, which I can use to pass or fail my tests. Although it's also possible that I was just too lazy to put in the time to figure out how to properly integrate the perfectly fine existing file diffing libraries with my tests.

### Stuff I Didn't Get To

I still had more goals for the quarter which I didn't have time for:
* Dumping assembly in addition to IR
    * I probably would've done this by changing the `--dump-ir` flag into a more general `--dump` flag with 3 options:
        1. `--dump ir`: IR as pseudo-assembly
        2. `--dump cfg` or `--dump ir-cfg`: IR as a Dot control-flow graph
        3. `--dump asm`: Assembly
* I would've liked to give the [flexstr](https://github.com/nu11ptr/flexstr) library a shot, because it seems to fit my use case pretty well.
* I played around with the idea of adding new language features, but never had time to get anywhere:
    * Logical operators for conditions
    * Bitwise logical operators
    * Shift operators
    * Break and continue for loops
    * Else-if branches for if statements
* I had the idea of experimenting with adding immediate instructions to IR directly (rather than always requiring constants to be allocated separately). This would've allowed me to detect only non-immediate uses of constants, which may have helped during register allocation.
    * There are also ways to detect that via analysis of existing instructions, so it's not the only way to go.

I also threw around the idea of writing a backend for a real target platform (e.g. ARM/macOS or x86/Linux), but never really got anywhere. For a brief moment I even wondered what would happen if I integrated with LLVM as a backend.

## License

This project is licensed under the MIT License.

Regardless of those terms, I'd just like to say explicitly: please don't use this code for academic dishonesty, or anything else that's unsavory. Besides, only crazy people would write this in Rust anyway, so your chances of successfully passing this code off as your own are pretty low. Basically, just don't do it.
