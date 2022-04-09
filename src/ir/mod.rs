mod gen;

use std::fmt::{self, Display, Formatter};
use crate::ast::Computation;
use self::gen::IrGenerator;

#[derive(Debug)]
pub struct IrStore {
    instrs: Vec<InstructionData>,
    blocks: Vec<BasicBlockData>,
}

impl IrStore {
    pub fn new() -> IrStore {
        IrStore {
            instrs: vec![],
            blocks: vec![],
        }
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
    StoredBinaryOp { opcode: StoredBinaryOpcode, dest: Value, src1: Value, src2: Value },
    Read(Value),
    Write(Value),
    Writeln,
    End,
    Nop,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BranchOpcode {
    Br, Beq, Bne, Bgt, Bge, Blt, Ble,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum StoredBinaryOpcode {
    Add, Sub, Nul, Div, Phi,
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

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Value(usize);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}
