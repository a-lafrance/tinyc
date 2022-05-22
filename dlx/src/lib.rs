pub mod isa;
pub mod utils;

use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, Read, Write},
};
use self::isa::{F1Opcode, F2Opcode, Instruction, InstrDecodeError, Register};

const MEM_SIZE: usize = 256;

pub struct Emulator<Stdin: Read, Stdout: Write> {
    registers: [u32; Register::N_REGS],
    instr_mem: Vec<Instruction>,
    data_mem: [u32; MEM_SIZE],
    pc: usize,
    stdin: BufReader<Stdin>,
    stdout: Stdout,
    quiet: bool,
}

impl<Stdin: Read, Stdout: Write> Emulator<Stdin, Stdout> {
    pub fn load(prog_file: &str, stdin: Stdin, stdout: Stdout, quiet: bool) -> Result<Emulator<Stdin, Stdout>, LoadError> {
        // open file/bufreader for file
        let f = File::open(prog_file)?;
        let mut reader = BufReader::new(f);

        // init instr memory to a large buffer
        let mut instr_mem = Vec::with_capacity(64);
        let mut buf = [0u8; 4]; // allocate a 4-byte buffer for each instruction

        // read 4 byte chunks and decode instructions until eof
        while let Some(instr) = utils::read_instr(&mut reader, &mut buf)? {
            instr_mem.push(instr);
        }

        let mut e = Emulator {
            registers: [0; Register::N_REGS],
            instr_mem,
            data_mem: [0; MEM_SIZE],
            pc: 0,
            stdin: BufReader::new(stdin),
            stdout,
            quiet,
        };

        let stack_start = e.data_mem.len() as u32 - 1;
        e.store_reg(Register::RSP, stack_start);
        e.store_reg(Register::RFP, stack_start);

        Ok(e)
    }

    // TODO: return some kind of result/exit status
    pub fn start(&mut self) {
        loop {
            match self.exec_current_instr() {
                ControlFlow::Continue => (),
                ControlFlow::Branch(offset) => {
                    let pc = &mut (self.pc as isize);
                    *pc += offset as isize;
                }
                ControlFlow::Quit => break,
            }
        }
    }

    fn exec_current_instr(&mut self) -> ControlFlow {
        match self.fetch_instr() {
            Instruction::F1(F1Opcode::Addi, dest, src, imm) => {
                let result = (self.load_reg(src) as i64 + imm as i64) as u32;
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Subi, dest, src, imm) => {
                let result = (self.load_reg(src) as i64 - imm as i64) as u32;
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Muli, dest, src, imm) => {
                let result = (self.load_reg(src) as i64 * imm as i64) as u32;
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Divi, dest, src, imm) => {
                let result = (self.load_reg(src) as i64 / imm as i64) as u32;
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Cmpi, dest, src, imm) => {
                let src = self.load_reg(src) as i64;
                let result = match src.cmp(&(imm as i64)) {
                    Ordering::Less => 0,
                    Ordering::Equal => 1,
                    Ordering::Greater => 2,
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Ldw, dest, base, offset) => {
                let addr = (self.load_reg(base) as i32) + (offset as i32);
                self.load_mem_into_reg(addr as u32, dest);
                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Pop, dest, sp, size) => {
                // load from sp into dest
                // inc sp by size
                self.load_mem_into_reg(self.load_reg(sp), dest);
                self.update_sp(sp, size);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Stw, src, base, offset) => {
                let addr = (self.load_reg(base) as i32) + (offset as i32);
                self.store_mem_from_reg(addr as u32, src);
                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Psh, src, sp, size) => {
                self.update_sp(sp, size);
                self.store_mem_from_reg(self.load_reg(sp), src);

                ControlFlow::Continue
            },
            Instruction::F1(F1Opcode::Beq, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) == 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bne, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) != 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Ble, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) <= 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bgt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) > 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Blt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) < 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Bge, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) >= 1 {
                    ControlFlow::Branch(offset)
                } else {
                    ControlFlow::Continue
                }
            },
            Instruction::F1(F1Opcode::Wrl, _, _, _) => {
                self.writeln();
                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Add, dest, src1, src2) => {
                let result = self.load_reg(src1) + self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Sub, dest, src1, src2) => {
                let result = self.load_reg(src1) - self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Mul, dest, src1, src2) => {
                let result = self.load_reg(src1) * self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Div, dest, src1, src2) => {
                let result = self.load_reg(src1) / self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Cmp, dest, lhs, rhs) => {
                let result = match lhs.cmp(&rhs) {
                    Ordering::Less => 0,
                    Ordering::Equal => 1,
                    Ordering::Greater => 2,
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            },
            Instruction::F2(F2Opcode::Ret, _, _, Register::R0) => ControlFlow::Quit,
            Instruction::F2(F2Opcode::Ret, _, _, _dest) => todo!(),
            Instruction::F2(F2Opcode::Rdd, dest, _, _) => {
                self.read_to(dest);
                ControlFlow::Continue
            }
            Instruction::F2(F2Opcode::Wrd, _, src, _) => {
                self.write_from(src);
                ControlFlow::Continue
            },
            Instruction::F3(_, _) => unimplemented!("F3 instructions not yet implemented"),
        }
    }

    fn fetch_instr(&mut self) -> Instruction {
        let instr = self.instr_mem[self.pc];
        self.pc += 1;

        instr
    }

    fn load_reg(&self, r: Register) -> u32 {
        self.registers[r.0 as usize]
    }

    fn store_reg(&mut self, r: Register, val: u32) {
        self.registers[r.0 as usize] = val;
    }

    fn load_mem(&self, addr: u32) -> u32 {
        self.data_mem[addr as usize]
    }

    fn store_mem(&mut self, addr: u32, val: u32) {
        self.data_mem[addr as usize] = val;
    }

    fn load_mem_into_reg(&mut self, addr: u32, dest: Register) {
        let data = self.load_mem(addr);
        self.store_reg(dest, data);
    }

    fn store_mem_from_reg(&mut self, addr: u32, src: Register) {
        self.store_mem(addr, self.load_reg(src));
    }

    fn update_sp(&mut self, sp: Register, offset: i16) {
        let current_sp = self.load_reg(sp);
        let new_sp = (current_sp as i32) + (offset as i32);
        self.store_reg(sp, new_sp as u32);
    }

    fn read_to(&mut self, r: Register) {
        if !self.quiet {
            write!(self.stdout, "> ").unwrap();
            self.stdout.flush().unwrap();
        }

        let mut buf = String::new();
        self.stdin.read_line(&mut buf).expect("failed to read integer");

        let val = buf.trim().parse::<u32>().expect("invalid input");
        self.store_reg(r, val);
    }

    fn write_from(&mut self, r: Register) {
        write!(self.stdout, "{}", self.load_reg(r)).expect("failed to write integer");
    }

    fn writeln(&mut self) {
        writeln!(self.stdout).expect("failed to write newline");
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlFlow {
    Continue,
    Branch(i16),
    Quit,
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
