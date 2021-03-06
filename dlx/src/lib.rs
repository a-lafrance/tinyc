pub mod isa;
pub mod utils;

use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, Read, Write},
    path::Path,
};
use self::isa::{F1Opcode, F2Opcode, F3Opcode, Instruction, InstrDecodeError, Register};

const MEM_SIZE: usize = 1024;
const GLOBAL_DATA_BYTES: usize = 128;

pub struct Emulator<Stdin: Read, Stdout: Write> {
    registers: [i32; Register::N_REGS],
    instr_mem: Vec<Instruction>,
    data_mem: [u8; MEM_SIZE],
    pc: usize,
    stdin: BufReader<Stdin>,
    stdout: Stdout,
    quiet: bool,
}

impl<Stdin: Read, Stdout: Write> Emulator<Stdin, Stdout> {
    pub fn load(
        prog_file: impl AsRef<Path>,
        stdin: Stdin,
        stdout: Stdout,
        quiet: bool,
    ) -> Result<Emulator<Stdin, Stdout>, LoadError> {
        // open file/bufreader for file
        let f = File::open(prog_file.as_ref())?;
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

        let base_addr = utils::word_to_byte_addr(e.data_mem.len() - 1);
        e.store_reg(Register::RDP, base_addr as i32);

        let stack_start = base_addr - GLOBAL_DATA_BYTES;
        e.store_reg(Register::RSP, stack_start as i32);
        e.store_reg(Register::RFP, stack_start as i32);

        Ok(e)
    }

    // TODO: return some kind of result/exit status
    pub fn start(&mut self) {
        loop {
            match self.exec_current_instr() {
                ControlFlow::Continue => self.pc += Instruction::SIZE_IN_BYTES,
                ControlFlow::Quit => break,

                ControlFlow::Branch(offset) => {
                    let mut pc = self.pc as isize;
                    pc += offset as isize;

                    self.pc = pc as usize;
                }

                ControlFlow::Jump(dest) => self.pc = dest,
            }
        }
    }

    fn exec_current_instr(&mut self) -> ControlFlow {
        match self.fetch_instr() {
            Instruction::F1(F1Opcode::Addi, dest, src, imm) => {
                let result = self.load_reg(src).wrapping_add(imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Subi, dest, src, imm) => {
                let result = self.load_reg(src).wrapping_sub(imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Muli, dest, src, imm) => {
                let result = self.load_reg(src).wrapping_mul(imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Divi, dest, src, imm) => {
                let result = self.load_reg(src).wrapping_div(imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Modi, dest, src, imm) => {
                let result = self.load_reg(src) % (imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Cmpi, dest, src, imm) => {
                let result = match self.load_reg(src).cmp(&(imm as i32)) {
                    Ordering::Equal => -1,
                    Ordering::Less => 0,
                    Ordering::Greater => 1,
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Ori, dest, src, imm) => {
                let result = self.load_reg(src) | (imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Andi, dest, src, imm) => {
                let result = self.load_reg(src) & (imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Bici, dest, src, imm) => {
                let result = self.load_reg(src) & !(imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Xori, dest, src, imm) => {
                let result = self.load_reg(src) ^ (imm as i32);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Lshi, dest, src, imm) => {
                let result = if imm > 0 {
                    (self.load_reg(src) as u32) << (imm as u32)
                } else {
                    (self.load_reg(src) as u32) >> (-imm as u32)
                };
                self.store_reg(dest, result as i32);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Ashi, dest, src, imm) => {
                let result = if imm > 0 {
                    self.load_reg(src) << (imm as i32)
                } else {
                    self.load_reg(src) >> (-imm as i32)
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Chki, src, _, imm) => {
                let src = self.load_reg(src);

                if src < 0 || src >= (imm as i32) {
                    ControlFlow::Quit
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Ldw, dest, base, offset) => {
                let addr = self.load_reg(base) + (offset as i32);
                self.load_mem_into_reg((addr as u32) as usize, dest);
                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Pop, dest, sp, size) => {
                self.load_mem_into_reg((self.load_reg(sp) as u32) as usize, dest);
                self.update_sp(sp, size);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Stw, src, base, offset) => {
                let addr = self.load_reg(base) + (offset as i32);
                self.store_mem_from_reg((addr as u32) as usize, src);
                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Psh, src, sp, size) => {
                self.update_sp(sp, size);
                self.store_mem_from_reg((self.load_reg(sp) as u32) as usize, src);

                ControlFlow::Continue
            }

            Instruction::F1(F1Opcode::Beq, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) == 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Bne, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) != 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Ble, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) <= 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Bgt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) > 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Blt, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) < 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Bge, cmp_reg, _, offset) => {
                if self.load_reg(cmp_reg) >= 0 {
                    ControlFlow::Branch(offset as isize)
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F1(F1Opcode::Bsr, _, _, offset) => {
                self.save_jump_dest();
                ControlFlow::Branch(offset as isize)
            }

            Instruction::F1(F1Opcode::Wrl, _, _, _) => {
                self.writeln();
                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Add, dest, src1, src2) => {
                let result = self.load_reg(src1) + self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Sub, dest, src1, src2) => {
                let result = self.load_reg(src1) - self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Mul, dest, src1, src2) => {
                let result = self.load_reg(src1) * self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Div, dest, src1, src2) => {
                let result = self.load_reg(src1) / self.load_reg(src2);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Mod, dest, src, amt) => {
                let result = self.load_reg(src) % self.load_reg(amt);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Cmp, dest, lhs, rhs) => {
                let lhs = self.load_reg(lhs);
                let rhs = self.load_reg(rhs);
                let result = match lhs.cmp(&rhs) {
                    Ordering::Equal => 0,
                    Ordering::Less => -1,
                    Ordering::Greater => 1,
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Or, dest, lhs, rhs) => {
                let result = self.load_reg(lhs) | self.load_reg(rhs);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::And, dest, lhs, rhs) => {
                let result = self.load_reg(lhs) & self.load_reg(rhs);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Bic, dest, lhs, rhs) => {
                let result = self.load_reg(lhs) & !self.load_reg(rhs);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Xor, dest, lhs, rhs) => {
                let result = self.load_reg(lhs) ^ self.load_reg(rhs);
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Lsh, dest, src, amt) => {
                let amt = self.load_reg(amt);
                let result = if amt > 0 {
                    (self.load_reg(src) as u32) << (amt as u32)
                } else {
                    (self.load_reg(src) as u32) >> (-amt as u32)
                };
                self.store_reg(dest, result as i32);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Ash, dest, src, amt) => {
                let amt = self.load_reg(amt);
                let result = if amt > 0 {
                    self.load_reg(src) << amt
                } else {
                    self.load_reg(src) >> -amt
                };
                self.store_reg(dest, result);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Chk, src, _, offset) => {
                let src = self.load_reg(src);
                let offset = self.load_reg(offset);

                if src < 0 || src >= offset {
                    ControlFlow::Quit
                } else {
                    ControlFlow::Continue
                }
            }

            Instruction::F2(F2Opcode::Ldx, dest, base, offset) => {
                let addr = self.load_reg(base) + self.load_reg(offset);
                self.load_mem_into_reg((addr as u32) as usize, dest);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Stx, src, base, offset) => {
                let addr = self.load_reg(base) + self.load_reg(offset);
                self.store_mem_from_reg((addr as u32) as usize, src);

                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Ret, _, _, Register::R0) => ControlFlow::Quit,

            Instruction::F2(F2Opcode::Ret, _, _, dest) => {
                let dest_addr = self.load_reg(dest);
                ControlFlow::Jump(dest_addr as usize)
            }

            Instruction::F2(F2Opcode::Rdd, dest, _, _) => {
                self.read_to(dest);
                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Wrd, _, src, _) => {
                self.write_from(src);
                ControlFlow::Continue
            }

            Instruction::F2(F2Opcode::Wrh, _, src, _) => {
                self.write_hex_from(src);
                ControlFlow::Continue
            }

            Instruction::F3(F3Opcode::Jsr, dest) => {
                self.save_jump_dest();
                ControlFlow::Jump(dest as usize)
            }
        }
    }

    fn fetch_instr(&mut self) -> Instruction {
        let pc_index = utils::byte_to_word_addr(self.pc).expect("pc not word-aligned");
        self.instr_mem[pc_index]
    }

    fn load_reg(&self, r: Register) -> i32 {
        self.registers[r.0 as usize]
    }

    fn store_reg(&mut self, r: Register, val: i32) {
        if r.0 > 0 {
            self.registers[r.0 as usize] = val;
        }
    }

    fn load_mem(&self, addr: usize) -> i32 {
        i32::from_be_bytes(self.data_mem[addr..addr + 4].try_into().unwrap())
    }

    fn store_mem(&mut self, addr: usize, val: i32) {
        self.data_mem[addr..addr + 4].copy_from_slice(&val.to_be_bytes());
    }

    fn load_mem_into_reg(&mut self, addr: usize, dest: Register) {
        let data = self.load_mem(addr);
        self.store_reg(dest, data);
    }

    fn store_mem_from_reg(&mut self, addr: usize, src: Register) {
        self.store_mem(addr, self.load_reg(src));
    }

    fn update_sp(&mut self, sp: Register, offset: i16) {
        let new_sp = self.load_reg(sp) + (offset as i32);
        self.store_reg(sp, new_sp);
    }

    fn save_jump_dest(&mut self) {
        self.store_reg(Register::RRET, (self.pc as u32) as i32 + 1);
    }

    fn read_to(&mut self, r: Register) {
        if !self.quiet {
            write!(self.stdout, "> ").unwrap();
            self.stdout.flush().unwrap();
        }

        let mut buf = String::new();
        self.stdin.read_line(&mut buf).expect("failed to read integer");

        let val = buf.trim().parse::<i32>().expect("invalid input");
        self.store_reg(r, val);
    }

    fn write_from(&mut self, r: Register) {
        write!(self.stdout, "{}", self.load_reg(r)).expect("failed to write integer");
    }

    fn write_hex_from(&mut self, r: Register) {
        write!(self.stdout, "0x{:x}", self.load_reg(r)).expect("failed to write integer as hex");
    }

    fn writeln(&mut self) {
        writeln!(self.stdout).expect("failed to write newline");
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlFlow {
    Continue,
    Branch(isize),
    Jump(usize),
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
