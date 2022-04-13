use std::{
    collections::{hash_map::Iter as HashMapIter, HashMap},
    fmt::{self, Display, Formatter},
};
use crate::{
    ast::{FactorOp, TermOp},
    utils::RelOp,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Instruction(pub usize);

#[derive(Debug)]
pub enum InstrData {
    Const(u32, Value),
    Cmp(Value, Value),
    Branch(BranchOpcode, BasicBlock),
    StoredBinaryOp { opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value },
    Read(Value),
    Write(Value),
    Writeln,
    End,
    Nop,
}

impl Display for InstrData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstrData::Const(n, dest) => write!(f, "{} = const {}", dest, n),
            InstrData::Cmp(lhs, rhs) => write!(f, "cmp {}, {}", lhs, rhs),
            InstrData::Branch(opcode, dest) => write!(f, "{} {}", opcode, dest),
            InstrData::StoredBinaryOp { opcode, src1, src2, dest } => write!(f, "{} = {} {}, {}", dest, opcode, src1, src2),
            InstrData::Read(dest) => write!(f, "{} = read", dest),
            InstrData::Write(src) => write!(f, "write {}", src),
            InstrData::Writeln => write!(f, "writeln"),
            InstrData::End => write!(f, "end"),
            InstrData::Nop => write!(f, "nop"),
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

impl From<RelOp> for BranchOpcode {
    fn from(op: RelOp) -> BranchOpcode {
        match op {
            RelOp::Eq => BranchOpcode::Beq,
            RelOp::Ne => BranchOpcode::Bne,
            RelOp::Gt => BranchOpcode::Bgt,
            RelOp::Ge => BranchOpcode::Bge,
            RelOp::Lt => BranchOpcode::Blt,
            RelOp::Le => BranchOpcode::Ble,
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
pub struct BasicBlock(pub usize);

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

#[derive(Debug)]
pub struct BasicBlockData {
    body: Vec<Instruction>,
    val_table: HashMap<String, Value>,
    fallthrough_dest: Option<BasicBlock>,
    branch_dest: Option<BasicBlock>,
}

impl BasicBlockData {
    pub fn new() -> BasicBlockData {
        BasicBlockData::with_vals(HashMap::new())
    }

    pub fn new_from(bb: &BasicBlockData) -> BasicBlockData {
        BasicBlockData::with_vals(bb.val_table.clone())
    }

    fn with_vals(val_table: HashMap<String, Value>) -> BasicBlockData {
        BasicBlockData {
            body: vec![],
            val_table,
            fallthrough_dest: None,
            branch_dest: None,
        }
    }

    pub fn get_val(&self, var: &str) -> Option<Value> {
        self.val_table.get(var).copied()
    }

    pub fn assign(&mut self, var: String, val: Value) {
        self.val_table.insert(var, val);
    }

    pub fn values(&self) -> ValueIter<'_> {
        ValueIter(self.val_table.iter())
    }

    pub fn push_instr(&mut self, instr: Instruction) {
        self.body.push(instr);
    }

    pub fn set_fallthrough_dest(&mut self, dest: BasicBlock) {
        self.fallthrough_dest = Some(dest);
    }

    pub fn set_branch_dest(&mut self, dest: BasicBlock) {
        self.branch_dest = Some(dest);
    }
}


#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Value(pub usize);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

pub struct ValueIter<'a>(HashMapIter<'a, String, Value>);

impl<'a> Iterator for ValueIter<'a> {
    type Item = <HashMapIter<'a, String, Value> as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
