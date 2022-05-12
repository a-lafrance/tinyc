use std::{fs, process::{Command, Output}};
use tinyc::driver;
use uuid::Uuid;

pub struct TestRun {
    binary: String,
}

impl TestRun {
    // These functions _will_ panic, in order to terminate the test
    pub fn start(src_file: &str, id: Uuid) -> TestRun {
        // tinyc --arch dlx -o e2e-tmp-<id> <src_file>
        let binary = format!("/tmp/e2e-tmp-{}", id);
        driver::start(vec!["tinyc", "--arch", "dlx", "-o", &binary, src_file].into_iter());

        TestRun { binary }
    }

    pub fn check_output(&self, expected: &str) {
        // python3 rsrc/dlx.py <binary>
        let run_result = Command::new("python3")
            .args(["rsrc/dlx.py", &self.binary])
            .output()
            .expect("failed to execute test binary");
        let output = String::from_utf8(run_result.stdout)
            .expect("invalid stdout received from test binary");

        assert!(run_result.status.success());
        assert_eq!(&output, expected);
    }
}

impl Drop for TestRun {
    fn drop(&mut self) {
        // clean up, delete test binary
        fs::remove_file(&self.binary).expect("failed to delete test binary");
    }
}
