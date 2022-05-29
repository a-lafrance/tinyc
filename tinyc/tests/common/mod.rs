pub mod codegen;

use std::{
    ffi::OsStr,
    fs::{self, File},
    io::Read,
    path::PathBuf,
};
use dlx::Emulator;
use tinyc::driver;
use uuid::Uuid;


pub fn run_e2e_test(
    id: Uuid,
    test_path: &str,
    opt_args: Option<Vec<&OsStr>>,
) {
    // FIXME: pretty sure all the clones can be optimized away
    let test_tmp_dir = PathBuf::from(format!("/tmp/e2e-tmp-{}", id));
    fs::create_dir(&test_tmp_dir).unwrap();

    let test_path = PathBuf::from(test_path);
    let mut src_file_path = test_path.clone();
    src_file_path.set_extension("tiny");

    // if output file exists, compile & run as DLX binary
    let mut output_file_path = test_path.clone();
    output_file_path.set_extension("out");

    if output_file_path.exists() {
        // tinyc --arch dlx -o <test_bin_path> <src_file_path>
        // examine output vs expected output
        let mut test_bin_path = test_tmp_dir.clone();
        test_bin_path.push("test.bin");

        let mut args = vec![
            OsStr::new("tinyc"),
            OsStr::new("--arch"),
            OsStr::new("dlx"),
            OsStr::new("-o"),
            test_bin_path.as_os_str(),
            src_file_path.as_os_str(),
        ];

        if let Some(ref opt_args) = opt_args {
            args.extend(opt_args.iter());
        }

        // TODO: get some kind of exit status from the compiler
        driver::start(args.into_iter());

        let mut input_file_path = test_path.clone();
        input_file_path.set_extension("in");

        let mut input_file = if input_file_path.exists() {
            File::open(&input_file_path).unwrap()
        } else {
            File::open("/dev/null").unwrap()
        };

        let mut test_output_file_path = test_tmp_dir.clone();
        test_output_file_path.push("test.out");
        let mut test_output_file = File::create(&test_output_file_path).unwrap();

        Emulator::load(&test_bin_path, &mut input_file, &mut test_output_file, true)
            .expect("failed to load emulator")
            .start();

        let mut test_output_file = File::open(&test_output_file_path).unwrap();
        let mut test_output = String::new();
        test_output_file.read_to_string(&mut test_output).unwrap();

        let mut exp_output_file = File::open(&output_file_path).unwrap();
        let mut exp_output = String::new();
        exp_output_file.read_to_string(&mut exp_output).unwrap();

        assert_eq!(test_output.trim(), exp_output.trim());
    }

    // if ir file exists, dump ir
    let mut ir_file_path = test_path.clone();
    ir_file_path.set_extension("ir");

    if ir_file_path.exists() {
        // tinyc --dump-ir=text -o <prefix>
        // dump ir & examine
        let mut test_ir_file_path = test_tmp_dir.clone();
        test_ir_file_path.push("test.ir");

        let mut args = vec![
            OsStr::new("tinyc"),
            OsStr::new("--dump-ir"),
            OsStr::new("text"),
            OsStr::new("-o"),
            test_ir_file_path.as_os_str(),
            src_file_path.as_os_str(),
        ];

        if let Some(ref opt_args) = opt_args {
            args.extend(opt_args.iter());
        }

        // TODO: exit status from compiler
        driver::start(args.into_iter());

        let mut test_ir_file = File::open(&test_ir_file_path).unwrap();
        let mut test_ir_dump = String::new();
        test_ir_file.read_to_string(&mut test_ir_dump).unwrap();

        let mut exp_ir_file = File::open(&ir_file_path).unwrap();
        let mut exp_ir_dump = String::new();
        exp_ir_file.read_to_string(&mut exp_ir_dump).unwrap();

        assert_eq!(test_ir_dump.trim(), exp_ir_dump.trim());
    }

    fs::remove_dir_all(&test_tmp_dir).unwrap();
}
