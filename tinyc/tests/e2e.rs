mod common;

use std::borrow::Cow;
use common::TestRun;
use uuid::Uuid;

#[test]
fn e2e_sanity_check() {
    TestRun::start("rsrc/const_prop.tiny", Uuid::new_v4()).run(String::new(), "14\n");
}
