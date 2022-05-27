pub mod codegen;

use std::{
    fs::{self, File},
    io::Read,
};
use dlx::Emulator;
use tinyc::driver;
use uuid::Uuid;

pub struct TestRun {
    prefix: String,
    binary: String,
}

impl TestRun {
    // These functions _will_ panic, in order to terminate the test
    pub fn start(src_file: &str, id: Uuid, opt_args: Option<Vec<&str>>) -> TestRun {
        // tinyc --arch dlx -o e2e-tmp-<id> <src_file>
        let prefix = format!("/tmp/e2e-tmp-{}", id);
        let binary = format!("{}.bin", prefix);
        let mut args = vec!["tinyc", "--arch", "dlx", "-o", &binary, src_file];

        if let Some(opt_args) = opt_args {
            args.extend(opt_args.iter());
        }

        driver::start(args.into_iter());
        TestRun { prefix, binary }
    }

    pub fn run(self, input_file: &str, exp_output_file: &str) {
        let output_file_name = format!("{}.out", self.prefix);

        let mut input_file = File::open(input_file).or(File::open("/dev/null")).unwrap();
        let mut output_file = File::create(&output_file_name).unwrap();

        // TODO: some kind of timeout mechanism
        Emulator::load(&self.binary, &mut input_file, &mut output_file, true)
            .expect("failed to start emulator")
            .start();

        let mut output_file = File::open(&output_file_name).unwrap();
        let mut output = String::new();
        output_file.read_to_string(&mut output).unwrap();

        let mut exp_output_file = File::open(exp_output_file).unwrap();
        let mut exp_output = String::new();
        exp_output_file.read_to_string(&mut exp_output).unwrap();

        assert_eq!(output, exp_output);
    }
}

impl Drop for TestRun {
    fn drop(&mut self) {
        // clean up, delete test binary
        // TODO: delete other files that were created
            // but also maybe not
        fs::remove_file(&self.binary).expect("failed to delete test binary");
    }
}
