pub mod isa;

use std::io;
use self::isa::{Instruction, InstrDecodeError};

pub struct Emulator {
    registers: [u32; 32],
    instrs: Vec<Instruction>,
    pc: usize,
}

impl Emulator {
    pub fn load(prog_file: &str) -> Result<Emulator, LoadError> {
        // open file/bufreader for file
        // init instr memory to a large buffer
        // until eof:
            // read u32
            // try to decode instruction
            // append to instr memory

        // init registers to 0
        // init instr memory
        // set pc to 0
        todo!();
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum LoadError {
    InstrDecodeFailed(InstrDecodeError),
    SystemFailure(io::Error),
}

impl Display for LoadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "failed to load program into emulator: ")?;

        match self {
            LoadError::InstrDecodeFailed(e) => write!(f, "{}", e),
            LoadError::SystemFailure(e) => write(f, "{}", e),
        }
    }
}

impl Error for LoadError { }
