use std::{
    fs::File,
    io::{BufReader, Write},
};
use clap::Parser;
use dlx::{isa::Instruction, utils};

#[derive(Parser)]
struct Args {
    #[clap(help = "DLX binary to display")]
    binary: String,

    #[clap(short, long, help = "Output file (defaults to stdout)")]
    output: Option<String>,
}

fn print_instr(outfile: Option<&mut File>, instr: &Instruction) {
    match outfile {
        Some(f) => {
            write!(f, "{}", instr).unwrap();
        }

        None => {
            println!("{}", instr);
        }
    }
}

fn main() {
    let args = Args::parse();
    let mut outfile = args.output.map(|path| File::create(&path).unwrap());
    let mut reader = BufReader::new(File::open(&args.binary).unwrap());
    let mut buf = [0u8; 4];

    while let Some(instr) = utils::read_instr(&mut reader, &mut buf).unwrap() {
        print_instr(outfile.as_mut(), &instr);
    }
}
