pub mod isa;

use std::{
    fmt::{self, Display, Formatter},
    error::Error,
    fs::File,
    io::{self, BufReader, Read, Write},
};
use self::isa::{F1Opcode, F2Opcode, Instruction, InstrDecodeError, Register};

pub struct Emulator<Stdout: Write> {
    registers: [u32; Register::N_REGS],
    instr_mem: Vec<Instruction>,
    pc: usize,
    stdout: Stdout,
}

impl<Stdout: Write> Emulator<Stdout> {
    pub fn load(prog_file: &str, stdout: Stdout) -> Result<Emulator<Stdout>, LoadError> {
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
            stdout,
        })
    }

    pub fn start(&mut self) {
        loop {
            match self.exec_current_instr() {
                ControlFlow::Continue => self.pc += 1,
                ControlFlow::Branch(offset) => {
                    let pc = &mut (self.pc as isize);
                    *pc += offset as isize;
                }
                ControlFlow::Quit => break,
            }
        }
    }

    fn exec_current_instr(&mut self) -> ControlFlow {
        match self.current_instr() {
            Instruction::F1(F1Opcode::Addi, r1, r2, imm) => {
                let result = (self.load_reg(r2) as i64 + imm as i64) as u32;
                self.store_reg(r1, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Beq, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) == 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bne, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) != 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Ble, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) <= 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bgt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) > 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Blt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) < 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bge, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) >= 0 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Wrl, _, _, _) => {
                self.writeln();
                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Ret, _, _, Register::R0) => ControlFlow::Quit,
            Instruction::F2(F2Opcode::Ret, _, _, dest) => todo!(),
            Instruction::F2(F2Opcode::Wrd, _, src, _) => {
                self.write(src);
                ControlFlow::Continue
            }
            Instruction::F2(opcode, r1, r2, r3) => todo!(),
            Instruction::F3(_, _) => unimplemented!("F3 instructions not yet implemented"),
        }
    }

    fn current_instr(&self) -> Instruction {
        self.instr_mem[self.pc]
    }

    fn load_reg(&self, r: Register) -> u32 {
        self.registers[r.0 as usize]
    }

    fn store_reg(&mut self, r: Register, val: u32) {
        self.registers[r.0 as usize] = val;
    }

    fn write(&mut self, r: Register) {
        write!(self.stdout, "{}", self.load_reg(r)).expect("failed to write integer");
    }

    fn writeln(&mut self) {
        writeln!(self.stdout).expect("failed to write newline");
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
        let stdout = io::stdout();
        Emulator::load(
            "/Users/lafrance/Dev/School/cs142b/tinyc/a.out",
            stdout.lock(),
        ).expect("failed to load emulator").start();
    }
}
