use bytes::{BufMut, Bytes, BytesMut};
use crate::ir::isa::{BranchOpcode, StoredBinaryOpcode};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Instruction {
    F1(F1Opcode, Register, Register, u16),
    F2(F2Opcode, Register, Register, Register),
    F3(F3Opcode, u32),
}

impl Instruction {
    const INSTR_LEN: usize = 4;

    const OPCODE_SHIFT: u8 = 26;
    const R1_SHIFT: u8 = 21;
    const R2_SHIFT: u8 = 16;

    const REG_MASK: u32 = 0x1F;
    const OPCODE_MASK: u32 = 0x3F;

    pub fn as_bytes(&self) -> Bytes {
        let mut buf = BytesMut::with_capacity(Instruction::INSTR_LEN);

        let instr_as_u32 = match self {
            Instruction::F1(opcode, r1, r2, imm) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | ((r1.0 as u32 & Instruction::REG_MASK) << Instruction::R1_SHIFT)
                    | ((r2.0 as u32 & Instruction::REG_MASK) << Instruction::R2_SHIFT)
                    | (*imm as u32)
            },

            Instruction::F2(opcode, r1, r2, r3) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | ((r1.0 as u32 & Instruction::REG_MASK) << Instruction::R1_SHIFT)
                    | ((r2.0 as u32 & Instruction::REG_MASK) << Instruction::R2_SHIFT)
                    | (r3.0 as u32 & Instruction::REG_MASK)
            },

            Instruction::F3(opcode, imm) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT) | (imm & 0x03FFFFFF)
            },
        };

        buf.put_u32(instr_as_u32);
        buf.freeze()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum F1Opcode {
    Addi = 16, Subi, Muli, Divi, Cmpi = 21,
    Ldw = 32, Pop = 34, Stw = 36, Psh = 38,
    Beq = 40, Bne, Blt, Bge, Ble, Bgt, Wrl = 53
}

impl F1Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, F1Opcode::Beq | F1Opcode::Bne | F1Opcode::Blt | F1Opcode::Bge | F1Opcode::Ble | F1Opcode::Bgt)
    }
}

impl From<BranchOpcode> for F1Opcode {
    fn from(opcode: BranchOpcode) -> Self {
        match opcode {
            BranchOpcode::Br | BranchOpcode::Beq => F1Opcode::Beq,
            BranchOpcode::Bne => F1Opcode::Bne,
            BranchOpcode::Bgt => F1Opcode::Bgt,
            BranchOpcode::Bge => F1Opcode::Bge,
            BranchOpcode::Blt => F1Opcode::Blt,
            BranchOpcode::Ble => F1Opcode::Ble,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum F2Opcode {
    Add, Sub, Mul, Div, Cmp = 5, Ldx = 33, Stx = 37, Ret = 49, Rdd = 50, Wrd,
}

impl F2Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }
}

impl From<StoredBinaryOpcode> for F2Opcode {
    fn from(opcode: StoredBinaryOpcode) -> Self {
        match opcode {
            StoredBinaryOpcode::Add => F2Opcode::Add,
            StoredBinaryOpcode::Sub => F2Opcode::Sub,
            StoredBinaryOpcode::Mul => F2Opcode::Mul,
            StoredBinaryOpcode::Div => F2Opcode::Div,
            StoredBinaryOpcode::Phi => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum F3Opcode {
    Jsr = 48
}

impl F3Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }
}

// enum?
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Register(pub u8);

impl Register {
    pub const R0: Register = Register(0);
    pub const RCMP: Register = Register(27);
}
