mod common;

use std::borrow::Cow;
use common::TestRun;
use uuid::Uuid;

// TODO: codegen for automatic e2e test generation
// NOTE: e2e tests should verify 2 things:
    // 1) the ir generated by the compiler
        // to verify this, don't actually compare the ir objects, just use the --dump-ir=text flag and do a string comparison
    // 2) the output of the binary when run in the emulator

// here's the arithmetic, done with the crazy codegen thing:
/*
    define_e2e_test!(const_prop, "--enable-const-prop")
*/
// which generates a test named `e2e_const_prop` that:
    // compiles the input file tests/rsrc/const_prop.tiny with the flag --enable-const-prop
    // checks the ir against the file tests/rsrc/const_prop.ir, if it exists
    // checks the binary output against the file tests/rsrc/const_prop.out, if it exists

// for ir optimization tests (and generally just optimization-type tests), just make them e2e tests rather than unit tests

#[test]
fn e2e_const_prop() {
    TestRun::start("rsrc/const_prop.tiny", Uuid::new_v4(), Some(vec!["--enable-const-prop"]))
        .run(String::new(), "14\n");
}

#[test]
fn e2e_arithmetic() {
    TestRun::start("rsrc/arithmetic.tiny", Uuid::new_v4(), None)
        .run("10\n5\n".to_string(), "15\n5\n50\n2\n");
}
