use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use bytes::{BufMut, Bytes, BytesMut};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Instruction {
    F1(F1Opcode, Register, Register, i16),
    F2(F2Opcode, Register, Register, Register),
    F3(F3Opcode, u32),
}

impl Instruction {
    const INSTR_LEN: usize = 4;

    const OPCODE_SHIFT: u8 = 26;
    const R1_SHIFT: u8 = 21;
    const R2_SHIFT: u8 = 16;

    const OPCODE_MASK: u32 = 0x3F;
    const REG_MASK: u32 = 0x1F;
    const F3_IMM_MASK: u32 = 0x03FFFFFF;

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
                ((opcode.as_bytes() & Instruction::OPCODE_MASK) << Instruction::OPCODE_SHIFT)
                    | (imm & Instruction::F3_IMM_MASK)
            },
        };

        buf.put_u32(instr_as_u32);
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

        if let Ok(opcode) = F1Opcode::try_from(opcode_bits) {
            let r1_bits = (bytes >> Instruction::R1_SHIFT) & Instruction::REG_MASK;
            let r2_bits = (bytes >> Instruction::R2_SHIFT) & Instruction::REG_MASK;

            Ok(Instruction::F1(
                opcode,
                Register::try_from(r1_bits as u8)?,
                Register::try_from(r2_bits as u8)?,
                bytes as i16,
            ))
        } else if let Ok(opcode) = F2Opcode::try_from(opcode_bits) {
            let r1_bits = (bytes >> Instruction::R1_SHIFT) & Instruction::REG_MASK;
            let r2_bits = (bytes >> Instruction::R2_SHIFT) & Instruction::REG_MASK;
            let r3_bits = bytes & Instruction::REG_MASK;

            Ok(Instruction::F2(
                opcode,
                Register::try_from(r1_bits as u8)?,
                Register::try_from(r2_bits as u8)?,
                Register::try_from(r3_bits as u8)?,
            ))
        } else if let Ok(opcode) = F3Opcode::try_from(opcode_bits) {
            Ok(Instruction::F3(opcode, bytes & Instruction::F3_IMM_MASK))
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


#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum F1Opcode {
    Addi = 16, Subi, Muli, Divi, Cmpi = 21, Pop = 34, Psh = 38, Beq = 40, Bne, Blt, Bge, Ble, Bgt, Wrl = 53
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
            F1Opcode::Cmpi => write!(f, "cmpi"),
            F1Opcode::Pop => write!(f, "pop"),
            F1Opcode::Psh => write!(f, "psh"),
            F1Opcode::Beq => write!(f, "beq"),
            F1Opcode::Bne => write!(f, "bne"),
            F1Opcode::Blt => write!(f, "blt"),
            F1Opcode::Bge => write!(f, "bge"),
            F1Opcode::Ble => write!(f, "ble"),
            F1Opcode::Bgt => write!(f, "bgt"),
            F1Opcode::Wrl => write!(f, "wrl"),
        }
    }
}

impl TryFrom<u8> for F1Opcode {
    type Error = InvalidOpcode;

    fn try_from(bits: u8) -> Result<Self, Self::Error> {
        match bits {
            16 => Ok(F1Opcode::Addi),
            17 => Ok(F1Opcode::Subi),
            18 => Ok(F1Opcode::Muli),
            19 => Ok(F1Opcode::Divi),
            21 => Ok(F1Opcode::Cmpi),
            34 => Ok(F1Opcode::Pop),
            38 => Ok(F1Opcode::Psh),
            40 => Ok(F1Opcode::Beq),
            41 => Ok(F1Opcode::Bne),
            42 => Ok(F1Opcode::Blt),
            43 => Ok(F1Opcode::Bge),
            44 => Ok(F1Opcode::Ble),
            45 => Ok(F1Opcode::Bgt),
            53 => Ok(F1Opcode::Wrl),
            other => Err(InvalidOpcode(other)),
        }
    }
}


#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum F2Opcode {
    Add, Sub, Mul, Div, Cmp = 5, Ret = 49, Rdd = 50, Wrd,
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
            F2Opcode::Cmp => write!(f, "cmp"),
            F2Opcode::Ret => write!(f, "ret"),
            F2Opcode::Rdd => write!(f, "rdd"),
            F2Opcode::Wrd => write!(f, "wrd"),
        }
    }
}

impl TryFrom<u8> for F2Opcode {
    type Error = InvalidOpcode;

    fn try_from(bits: u8) -> Result<Self, Self::Error> {
        match bits {
            0 => Ok(F2Opcode::Add),
            1 => Ok(F2Opcode::Sub),
            2 => Ok(F2Opcode::Mul),
            3 => Ok(F2Opcode::Div),
            5 => Ok(F2Opcode::Cmp),
            49 => Ok(F2Opcode::Ret),
            50 => Ok(F2Opcode::Rdd),
            51 => Ok(F2Opcode::Wrd),
            other => Err(InvalidOpcode(other)),
        }
    }
}


#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

impl TryFrom<u8> for F3Opcode {
    type Error = InvalidOpcode;

    fn try_from(bits: u8) -> Result<Self, Self::Error> {
        match bits {
            48 => Ok(F3Opcode::Jsr),
            other => Err(InvalidOpcode(other)),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Register(pub u8);

impl Register {
    pub const N_REGS: usize = 32;
    pub const R0: Register = Register(0);
    pub const RSP: Register = Register(29);
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
