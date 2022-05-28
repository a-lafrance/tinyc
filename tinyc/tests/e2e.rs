#[macro_use] mod common;

// which generates a test named `e2e_const_prop` that:
    // compiles the input file tests/rsrc/const_prop.tiny with the flag --enable-const-prop
    // checks the ir against the file tests/rsrc/const_prop.ir, if it exists
    // checks the binary output against the file tests/rsrc/const_prop.out, if it exists
    // could also add an option to check against the cfg output, rather than setting up separate unit tests for that
        // or as an alternate way of verifying that stuff

// for ir optimization tests (and generally just optimization-type tests), just make them e2e tests rather than unit tests

define_e2e_test!(arithmetic);
define_e2e_test!(branched_arithmetic);
define_e2e_test!(cmp_duplicate);
define_e2e_test!(const_branch);
define_e2e_test!(const_branch2);
define_e2e_test!(const_branch_else);
define_e2e_test!(const_prop_and_br);
define_e2e_test!(const_prop);
define_e2e_test!(const_prop2);
define_e2e_test!(dead_val);
define_e2e_test!(read_and_write);
