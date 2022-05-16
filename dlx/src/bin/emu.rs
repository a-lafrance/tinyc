use std::io;
use clap::Parser;
use dlx::Emulator;

#[derive(Parser)]
struct Args {
    #[clap(help = "DLX binary to run")]
    binary: String,

    #[clap(short, long, help = "Run emu in quiet mode")]
    quiet: bool,
}

fn main() {
    let args = Args::parse();
    let stdin = io::stdin();
    let stdout = io::stdout();

    Emulator::load(
        &args.binary,
        stdin.lock(),
        stdout.lock(),
        args.quiet,
    ).expect("failed to load emulator").start();
}
