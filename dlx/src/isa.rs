use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use bytes::{BufMut, Bytes, BytesMut};
use discrim::FromDiscriminant;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Instruction {
    F1(F1Opcode, Register, Register, i16),
    F2(F2Opcode, Register, Register, Register),
    F3(F3Opcode, i32),
}

impl Instruction {
    pub const SIZE_IN_BYTES: usize = 4;

    const OPCODE_SHIFT: u8 = 26;
    const R1_SHIFT: u8 = 21;
    const R2_SHIFT: u8 = 16;

    const OPCODE_MASK: u32 = 0x3F;
    const REG_MASK: u32 = 0x1F;
    const F1_IMM_MASK: u32 = 0xFFFF;
    const F3_IMM_MASK: u32 = 0x03FFFFFF;

    pub fn as_bytes(&self) -> Bytes {
        let mut buf = BytesMut::with_capacity(Instruction::SIZE_IN_BYTES);

        let instr_bytes = match self {
            Instruction::F1(opcode, r1, r2, imm) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | ((r1.0 as u32 & Instruction::REG_MASK) << Instruction::R1_SHIFT)
                    | ((r2.0 as u32 & Instruction::REG_MASK) << Instruction::R2_SHIFT)
                    | (*imm as u32 & Instruction::F1_IMM_MASK)
            },

            Instruction::F2(opcode, r1, r2, r3) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | ((r1.0 as u32 & Instruction::REG_MASK) << Instruction::R1_SHIFT)
                    | ((r2.0 as u32 & Instruction::REG_MASK) << Instruction::R2_SHIFT)
                    | (r3.0 as u32 & Instruction::REG_MASK)
            },

            Instruction::F3(opcode, imm) => {
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | ((*imm as u32) & Instruction::F3_IMM_MASK)
            },
        };

        buf.put_u32(instr_bytes);
        buf.freeze()
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instruction::F1(opcode @ F1Opcode::Wrl, _, _, _) => write!(f, "{}", opcode),
            Instruction::F1(opcode, r1, _, imm) if opcode.is_branch() => write!(f, "{} {}, {}", opcode, r1, imm),
            Instruction::F1(opcode, r1, r2, imm) => write!(f, "{} {}, {}, {}", opcode, r1, r2, imm),
            Instruction::F2(opcode @ F2Opcode::Ret, _, _, dest) => write!(f, "{} {}", opcode, dest),
            Instruction::F2(opcode @ F2Opcode::Rdd, dest, _, _) => write!(f, "{} {}", opcode, dest),
            Instruction::F2(opcode @ F2Opcode::Wrd, _, src, _) => write!(f, "{} {}", opcode, src),
            Instruction::F2(opcode, r1, r2, r3) => write!(f, "{} {}, {}, {}", opcode, r1, r2, r3),
            Instruction::F3(opcode, imm) => write!(f, "{} {}", opcode, imm),
        }
    }
}

impl TryFrom<u32> for Instruction {
    type Error = InstrDecodeError;

    fn try_from(bytes: u32) -> Result<Self, Self::Error> {
        let opcode_bits = (bytes >> Instruction::OPCODE_SHIFT) as u8;

        if let Ok(opcode) = F1Opcode::from_discriminant(opcode_bits) {
            let r1_bits = (bytes >> Instruction::R1_SHIFT) & Instruction::REG_MASK;
            let r2_bits = (bytes >> Instruction::R2_SHIFT) & Instruction::REG_MASK;

            Ok(Instruction::F1(
                opcode,
                Register::try_from(r1_bits as u8)?,
                Register::try_from(r2_bits as u8)?,
                bytes as i16,
            ))
        } else if let Ok(opcode) = F2Opcode::from_discriminant(opcode_bits) {
            let r1_bits = (bytes >> Instruction::R1_SHIFT) & Instruction::REG_MASK;
            let r2_bits = (bytes >> Instruction::R2_SHIFT) & Instruction::REG_MASK;
            let r3_bits = bytes & Instruction::REG_MASK;

            Ok(Instruction::F2(
                opcode,
                Register::try_from(r1_bits as u8)?,
                Register::try_from(r2_bits as u8)?,
                Register::try_from(r3_bits as u8)?,
            ))
        } else if let Ok(opcode) = F3Opcode::from_discriminant(opcode_bits) {
            Ok(Instruction::F3(opcode, (bytes & Instruction::F3_IMM_MASK) as i32))
        } else {
            Err(InvalidOpcode(opcode_bits).into())
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InstrDecodeError {
    InvalidOpcode(InvalidOpcode),
    InvalidReg(InvalidReg),
}

impl Display for InstrDecodeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstrDecodeError::InvalidOpcode(e) => write!(f, "{}", e),
            InstrDecodeError::InvalidReg(e) => write!(f, "{}", e),
        }
    }
}

impl Error for InstrDecodeError { }

impl From<InvalidOpcode> for InstrDecodeError {
    fn from(e: InvalidOpcode) -> Self {
        InstrDecodeError::InvalidOpcode(e)
    }
}

impl From<InvalidReg> for InstrDecodeError {
    fn from(e: InvalidReg) -> Self {
        InstrDecodeError::InvalidReg(e)
    }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InvalidOpcode(u8);

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "invalid opcode '{}'", self.0)
    }
}

impl Error for InvalidOpcode { }


#[derive(Clone, Copy, Debug, Eq, FromDiscriminant, PartialEq)]
#[repr(u8)]
pub enum F1Opcode {
    Addi = 16, Subi, Muli, Divi, Modi, Cmpi,
    Ori = 24, Andi, Bici, Xori, Lshi, Ashi, Chki,
    Ldw = 32, Pop = 34, Stw = 36, Psh = 38,
    Beq = 40, Bne, Blt, Bge, Ble, Bgt, Bsr, Wrl = 53
}

impl F1Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, F1Opcode::Beq | F1Opcode::Bne | F1Opcode::Blt | F1Opcode::Bge | F1Opcode::Ble | F1Opcode::Bgt)
    }
}

impl Display for F1Opcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            F1Opcode::Addi => write!(f, "addi"),
            F1Opcode::Subi => write!(f, "subi"),
            F1Opcode::Muli => write!(f, "muli"),
            F1Opcode::Divi => write!(f, "divi"),
            F1Opcode::Modi => write!(f, "modi"),
            F1Opcode::Cmpi => write!(f, "cmpi"),
            F1Opcode::Ori => write!(f, "ori"),
            F1Opcode::Andi => write!(f, "andi"),
            F1Opcode::Bici => write!(f, "bici"),
            F1Opcode::Xori => write!(f, "xori"),
            F1Opcode::Lshi => write!(f, "lshi"),
            F1Opcode::Ashi => write!(f, "ashi"),
            F1Opcode::Chki => write!(f, "chki"),
            F1Opcode::Ldw => write!(f, "ldw"),
            F1Opcode::Pop => write!(f, "pop"),
            F1Opcode::Stw => write!(f, "stw"),
            F1Opcode::Psh => write!(f, "psh"),
            F1Opcode::Beq => write!(f, "beq"),
            F1Opcode::Bne => write!(f, "bne"),
            F1Opcode::Blt => write!(f, "blt"),
            F1Opcode::Bge => write!(f, "bge"),
            F1Opcode::Ble => write!(f, "ble"),
            F1Opcode::Bgt => write!(f, "bgt"),
            F1Opcode::Bsr => write!(f, "bsr"),
            F1Opcode::Wrl => write!(f, "wrl"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, FromDiscriminant, PartialEq)]
#[repr(u8)]
pub enum F2Opcode {
    Add, Sub, Mul, Div, Mod, Cmp,
    Or = 8, And, Bic, Xor, Lsh, Ash, Chk,
    Ldx = 33, Stx = 37, Ret = 49, Rdd = 50, Wrd, Wrh,
}

impl F2Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }
}

impl Display for F2Opcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            F2Opcode::Add => write!(f, "add"),
            F2Opcode::Sub => write!(f, "sub"),
            F2Opcode::Mul => write!(f, "mul"),
            F2Opcode::Div => write!(f, "div"),
            F2Opcode::Mod => write!(f, "mod"),
            F2Opcode::Cmp => write!(f, "cmp"),
            F2Opcode::Or => write!(f, "or"),
            F2Opcode::And => write!(f, "and"),
            F2Opcode::Bic => write!(f, "bic"),
            F2Opcode::Xor => write!(f, "xor"),
            F2Opcode::Lsh => write!(f, "lsh"),
            F2Opcode::Ash => write!(f, "ash"),
            F2Opcode::Chk => write!(f, "chk"),
            F2Opcode::Ldx => write!(f, "ldx"),
            F2Opcode::Stx => write!(f, "stx"),
            F2Opcode::Ret => write!(f, "ret"),
            F2Opcode::Rdd => write!(f, "rdd"),
            F2Opcode::Wrd => write!(f, "wrd"),
            F2Opcode::Wrh => write!(f, "wrh"),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, FromDiscriminant, PartialEq)]
#[repr(u8)]
pub enum F3Opcode {
    Jsr = 48
}

impl F3Opcode {
    pub fn as_bytes(self) -> u32 {
        (self as u8) as u32
    }
}

impl Display for F3Opcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            F3Opcode::Jsr => write!(f, "jsr"),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Register(pub u8);

impl Register {
    pub const N_REGS: usize = 32;
    pub const R0: Register = Register(0);
    pub const RFP: Register = Register(28);
    pub const RSP: Register = Register(29);
    pub const RDP: Register = Register(30); // RDP = "data pointer register", pointer to global data
    pub const RRET: Register = Register(31);
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "R{}", self.0)
    }
}

impl TryFrom<u8> for Register {
    type Error = InvalidReg;

    fn try_from(bits: u8) -> Result<Self, Self::Error> {
        if (bits as usize) < Register::N_REGS {
            Ok(Register(bits))
        } else {
            Err(InvalidReg(bits))
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InvalidReg(u8);

impl Display for InvalidReg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "invalid register number {}", self.0)
    }
}
