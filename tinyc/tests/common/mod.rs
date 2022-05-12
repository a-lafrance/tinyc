use std::{fs, process::Command};
use dlx::Emulator;
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
        let mut output_stream = Vec::new();

        // TODO: timeout system
            // maybe launch in separate thread?
        Emulator::load(&self.binary, expected.as_bytes(), &mut output_stream)
            .expect("failed to start emulator")
            .start();

        let output = String::from_utf8(output_stream).expect("invalid output from binary");
        assert_eq!(&output, expected);
    }
}

impl Drop for TestRun {
    fn drop(&mut self) {
        // clean up, delete test binary
        fs::remove_file(&self.binary).expect("failed to delete test binary");
    }
}
