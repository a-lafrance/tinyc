use std::{fs, thread};
use dlx::Emulator;
use tinyc::driver;
use uuid::Uuid;

pub struct TestRun {
    binary: String,
}

impl TestRun {
    // These functions _will_ panic, in order to terminate the test
    pub fn start(src_file: &str, id: Uuid, opt_args: Option<Vec<&str>>) -> TestRun {
        // tinyc --arch dlx -o e2e-tmp-<id> <src_file>
        let binary = format!("/tmp/e2e-tmp-{}", id);
        let mut args = vec!["tinyc", "--arch", "dlx", "-o", &binary, src_file];

        if let Some(opt_args) = opt_args {
            args.extend(opt_args.iter());
        }

        driver::start(args.into_iter());
        TestRun { binary }
    }

    pub fn run(self, input: String, expected: &str) {
        // TODO: some kind of timeout mechanism
        let mut output_stream = Vec::new();
        Emulator::load(&self.binary, input.as_bytes(), &mut output_stream)
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
