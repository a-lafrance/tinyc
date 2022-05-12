mod common;

use std::borrow::Cow;
use common::TestRun;
use uuid::Uuid;

// TODO: codegen for automatic e2e test generation

#[test]
fn e2e_const_prop() {
    TestRun::start("rsrc/const_prop.tiny", Uuid::new_v4())
        .run(String::new(), "14\n");
}

#[test]
fn e2e_arithmetic() {
    TestRun::start("rsrc/arithmetic.tiny", Uuid::new_v4())
        .run("10\n5\n".to_string(), "15\n5\n50\n2\n");
}
