mod gen;

use std::{
    collections::{hash_map::Iter as HashMapIter, HashMap},
    fmt::{self, Display, Formatter},
};
use crate::{
    ast::{Computation, FactorOp, TermOp},
    utils::RelOp,
};
use self::gen::IrGenerator;

#[derive(Debug)]
pub struct IrStore {
    instrs: Vec<InstructionData>,
    blocks: Vec<BasicBlockData>,
    root: Option<BasicBlock>,
}

impl IrStore {
    pub fn new() -> IrStore {
        IrStore {
            instrs: vec![],
            blocks: vec![],
            root: None,
        }
    }

    pub fn root_block(&self) -> Option<BasicBlock> {
        self.root
    }

    pub fn set_root_block(&mut self, bb: BasicBlock) {
        self.root = Some(bb);
    }

    pub fn basic_block_data(&self, bb: BasicBlock) -> &BasicBlockData {
        &self.blocks[bb.0]
    }

    pub fn basic_block_data_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockData {
        &mut self.blocks[bb.0]
    }

    pub fn make_new_basic_block(&mut self) -> BasicBlock {
        self.push_basic_block(BasicBlockData::new())
    }

    pub fn make_new_basic_block_from(&mut self, parent: BasicBlock) -> BasicBlock {
        let bb = BasicBlockData::new_from(self.basic_block_data(parent));
        self.push_basic_block(bb)
    }

    fn push_basic_block(&mut self, bb: BasicBlockData) -> BasicBlock {
        self.blocks.push(bb);
        BasicBlock(self.blocks.len() - 1)
    }

    pub fn push_instr(&mut self, block: BasicBlock, instr_data: InstructionData) -> Instruction {
        self.instrs.push(instr_data);
        let instr = Instruction(self.instrs.len() - 1);
        self.blocks[block.0].push_instr(instr);

        instr
    }

    pub fn connect_via_fallthrough(&mut self, src: BasicBlock, dest: BasicBlock) {
        self.basic_block_data_mut(src).set_fallthrough_dest(dest);
    }

    pub fn connect_via_branch(&mut self, src: BasicBlock, dest: BasicBlock, branch_type: BranchOpcode) {
        self.basic_block_data_mut(src).set_branch_dest(dest);
        self.push_instr(src, InstructionData::Branch(branch_type, dest));
    }

    pub fn replace_val_in_block(&mut self, bb: BasicBlock, old_val: Value, new_val: Value) {
        let bb_data = self.basic_block_data(bb);
        let bb_body = bb_data.body().to_vec();

        for i in bb_body.into_iter() {
            self.instrs[i.0].replace_val(old_val, new_val);
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

impl InstructionData {
    fn replace_val(&mut self, current_val: Value, new_val: Value) {
        match self {
            InstructionData::Const(_, ref mut val) if *val == current_val => *val = new_val,
            InstructionData::Cmp(ref mut lhs, ref mut rhs) => if *lhs == current_val {
                *lhs = new_val;
            } else if *rhs == current_val {
                *rhs = new_val;
            },
            InstructionData::StoredBinaryOp { ref mut src1, ref mut src2, ref mut dest, .. } => if *src1 == current_val {
                *src1 = new_val;
            } else if *src2 == current_val {
                *src2 = new_val;
            } else if *dest == current_val {
                *dest = new_val;
            },
            InstructionData::Read(ref mut val) if *val == current_val => *val = new_val,
            InstructionData::Write(ref mut val) if *val == current_val => *val = new_val,
            _ => {},
        }
    }
}

impl Display for InstructionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstructionData::Const(n, dest) => write!(f, "{} = const {}", dest, n),
            InstructionData::Cmp(lhs, rhs) => write!(f, "cmp {}, {}", lhs, rhs),
            InstructionData::Branch(opcode, dest) => write!(f, "{} {}", opcode, dest),
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
pub struct BasicBlock(usize);

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

    pub fn body(&self) -> &[Instruction] {
        &self.body
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
pub struct Value(usize);

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
