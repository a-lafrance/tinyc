mod gen;

use std::fmt::{self, Display, Formatter};
use crate::ast::{Computation, FactorOp, TermOp};
use self::gen::IrGenerator;

#[derive(Debug)]
pub struct IrStore {
    instrs: Vec<InstructionData>,
    blocks: Vec<BasicBlockData>,
    // TODO: some way to specify the entry block, ie the cfg root
}

impl IrStore {
    pub fn new() -> IrStore {
        IrStore {
            instrs: vec![],
            blocks: vec![],
        }
    }

    pub fn make_new_basic_block(&mut self) -> BasicBlock {
        let block = BasicBlockData::new();
        self.blocks.push(block);

        BasicBlock(self.blocks.len() - 1)
    }

    pub fn push_instr(&mut self, block: BasicBlock, instr_data: InstructionData) -> Instruction {
        self.instrs.push(instr_data);
        let instr = Instruction(self.instrs.len() - 1);
        self.blocks[block.0].push_instr(instr);

        instr
    }
}

impl From<Computation> for IrStore {
    fn from(ast: Computation) -> IrStore {
        IrGenerator::gen(&ast)
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Instruction(usize);

#[derive(Debug)]
pub enum InstructionData {
    Const(u32),
    Cmp(Value, Value),
    Branch(BranchOpcode, BasicBlock),
    StoredBinaryOp { opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value },
    Read(Value),
    Write(Value),
    Writeln,
    End,
    Nop,
}

impl Display for InstructionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstructionData::Const(n) => write!(f, "const {}", n),
            InstructionData::Cmp(lhs, rhs) => write!(f, "cmp {}, {}", lhs, rhs),
            InstructionData::Branch(opcode, dest) => write!(f, "{} TODO", opcode),
            InstructionData::StoredBinaryOp { opcode, src1, src2, dest } => write!(f, "{} = {} {}, {}", dest, opcode, src1, src2),
            InstructionData::Read(dest) => write!(f, "{} = read", dest),
            InstructionData::Write(src) => write!(f, "write {}", src),
            InstructionData::Writeln => write!(f, "writeln"),
            InstructionData::End => write!(f, "end"),
            InstructionData::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BranchOpcode {
    Br, Beq, Bne, Bgt, Bge, Blt, Ble,
}

impl Display for BranchOpcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            BranchOpcode::Br => write!(f, "br"),
            BranchOpcode::Beq => write!(f, "beq"),
            BranchOpcode::Bne => write!(f, "bne"),
            BranchOpcode::Bgt => write!(f, "bgt"),
            BranchOpcode::Bge => write!(f, "bge"),
            BranchOpcode::Blt => write!(f, "blt"),
            BranchOpcode::Ble => write!(f, "ble"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum StoredBinaryOpcode {
    Add, Sub, Mul, Div, Phi,
}

impl Display for StoredBinaryOpcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            StoredBinaryOpcode::Add => write!(f, "add"),
            StoredBinaryOpcode::Sub => write!(f, "sub"),
            StoredBinaryOpcode::Mul => write!(f, "mul"),
            StoredBinaryOpcode::Div => write!(f, "div"),
            StoredBinaryOpcode::Phi => write!(f, "phi"),
        }
    }
}
impl From<FactorOp> for StoredBinaryOpcode {
    fn from(op: FactorOp) -> StoredBinaryOpcode {
        match op {
            FactorOp::Mul => StoredBinaryOpcode::Mul,
            FactorOp::Div => StoredBinaryOpcode::Div,
        }
    }
}

impl From<TermOp> for StoredBinaryOpcode {
    fn from(op: TermOp) -> StoredBinaryOpcode {
        match op {
            TermOp::Add => StoredBinaryOpcode::Add,
            TermOp::Sub => StoredBinaryOpcode::Sub,
        }
    }
}

// wrapper around interned index
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct BasicBlock(usize);

#[derive(Debug)]
pub struct BasicBlockData {
    body: Vec<Instruction>,
    fallthrough_dest: Option<BasicBlock>,
    branch_dest: Option<BasicBlock>,
}

impl BasicBlockData {
    pub fn new() -> BasicBlockData {
        BasicBlockData {
            body: vec![],
            fallthrough_dest: None,
            branch_dest: None,
        }
    }

    pub fn push_instr(&mut self, instr: Instruction) {
        self.body.push(instr);
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Value(usize);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}
