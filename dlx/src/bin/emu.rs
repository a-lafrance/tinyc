use std::io;
use clap::Parser;
use dlx::Emulator;

#[derive(Parser)]
struct Args {
    #[clap(help = "DLX binary to run")]
    binary: String,
}

fn main() {
    let args = Args::parse();
    let stdin = io::stdin();
    let stdout = io::stdout();

    Emulator::load(
        &args.binary,
        stdin.lock(),
        stdout.lock(),
    ).expect("failed to load emulator").start();
}
