pub mod isa;

use std::{
    fmt::{self, Display, Formatter},
    error::Error,
    fs::File,
    io::{self, BufReader, Read},
};
use self::isa::{Instruction, InstrDecodeError, Register};

pub struct Emulator {
    registers: [u32; Register::N_REGS],
    instr_mem: Vec<Instruction>,
    pc: usize,
}

impl Emulator {
    pub fn load(prog_file: &str) -> Result<Emulator, LoadError> {
        // open file/bufreader for file
        let f = File::open(prog_file)?;
        let mut reader = BufReader::new(f);

        // init instr memory to a large buffer
        let mut instr_mem = Vec::with_capacity(64);
        let mut buf = [0u8; 4]; // allocate a 4-byte buffer for each instruction

        // read 4 byte chunks and decode instructions until eof
        while reader.read(&mut buf)? > 0 {
            // NOTE: took a short cut here and ignored short counts
            let instr_bytes = u32::from_be_bytes(buf);
            let instr = Instruction::try_from(instr_bytes)?;
            instr_mem.push(instr);
        }

        Ok(Emulator {
            registers: [0; Register::N_REGS],
            instr_mem,
            pc: 0,
        })
    }

    pub fn start(&mut self) {
        loop {
            match self.exec_next_instr() {
                ControlFlow::Continue => self.pc += 1,
                ControlFlow::Branch(offset) => {
                    let pc = &mut (self.pc as isize);
                    *pc += offset as isize;
                }
                ControlFlow::Quit => break,
            }
        }
    }

    fn exec_next_instr(&mut self) -> ControlFlow {
        ControlFlow::Quit
    }
}


#[derive(Debug)]
pub enum LoadError {
    InstrDecodeFailed(InstrDecodeError),
    IoFailure(io::Error),
}

impl Display for LoadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "failed to load program into emulator: ")?;

        match self {
            LoadError::InstrDecodeFailed(e) => write!(f, "{}", e),
            LoadError::IoFailure(e) => write!(f, "{}", e),
        }
    }
}

impl Error for LoadError { }

impl From<InstrDecodeError> for LoadError {
    fn from(e: InstrDecodeError) -> Self {
        LoadError::InstrDecodeFailed(e)
    }
}

impl From<io::Error> for LoadError {
    fn from(e: io::Error) -> Self {
        LoadError::IoFailure(e)
    }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlFlow {
    Continue,
    Branch(i16),
    Quit,
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity_check() {
        let mut e = Emulator::load("/Users/lafrance/Dev/School/cs142b/tinyc/a.out").expect("failed to load emulator");
        e.start();
    }
}
