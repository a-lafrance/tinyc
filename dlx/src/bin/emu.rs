use std::io;
use dlx::Emulator;

fn main() {
    let stdin = io::stdin();
    let stdout = io::stdout();

    Emulator::load(
        "/Users/lafrance/Dev/School/cs142b/tinyc/a.out",
        stdin.lock(),
        stdout.lock(),
    ).expect("failed to load emulator").start();
}
