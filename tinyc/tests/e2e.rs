#[macro_use] mod common;
// for ir optimization tests (and generally just optimization-type tests), just make them e2e tests rather than unit tests

// it would really be nice if we could stick e2e in the name of the e2e test functions somehow
// currently we're relying on this weird module hack to get it in the path name

// TODO: test recursion

use std::ffi::OsStr;

mod e2e {
    use crate::*;

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
    define_e2e_test!(if_else_if); // TODO: simulation output (pending phi stuff) for this and a bunch below too
    define_e2e_test!(if_else_loop);
    define_e2e_test!(if_no_else_no_phis);
    define_e2e_test!(if_no_else_with_phis);
    define_e2e_test!(if_then_if);
    define_e2e_test!(if_then_loop);
    define_e2e_test!(if_with_else_with_phis);
    define_e2e_test!(instr_select, OsStr::new("-O"), OsStr::new("full"));
    define_e2e_test!(loop_no_phis);
    define_e2e_test!(loop_with_if);
    define_e2e_test!(loop_with_loop);
    define_e2e_test!(loop_with_phis);
    define_e2e_test!(read_and_write);
    define_e2e_test!(uninit_val);
}
