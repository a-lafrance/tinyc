use std::{
    collections::{hash_map::Iter as HashMapIter, HashMap},
    fmt::{self, Display, Formatter},
};
use crate::{
    ast::{FactorOp, TermOp},
    utils::RelOp,
};
use dlx::isa::{F1Opcode, F2Opcode};


#[derive(Debug, PartialEq)]
pub struct Body {
    pub(super) blocks: Vec<BasicBlockData>,
    pub(super) root: Option<BasicBlock>,
}

impl Body {
    pub fn new() -> Body {
        Body {
            blocks: vec![],
            root: None,
        }
    }

    #[cfg(test)]
    pub fn from(blocks: Vec<BasicBlockData>, root: Option<BasicBlock>) -> Body {
        Body { blocks, root }
    }

    pub fn blocks(&self) -> &[BasicBlockData] {
        &self.blocks
    }

    pub fn root_block(&self) -> Option<BasicBlock> {
        self.root
    }

    pub fn set_root_block(&mut self, bb: BasicBlock) {
        if let Some(current_root) = self.root {
            self.establish_dominance(bb, current_root);
        }

        self.root = Some(bb);
    }

    pub fn basic_block_data(&self, bb: BasicBlock) -> &BasicBlockData {
        &self.blocks[bb.0]
    }

    pub fn basic_block_data_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockData {
        &mut self.blocks[bb.0]
    }

    pub fn root_block_entry(&self) -> Option<(BasicBlock, &BasicBlockData)> {
        self.root.map(|r| (r, self.basic_block_data(r)))
    }

    pub fn val_in_bb(&self, bb: BasicBlock, var: &str) -> Option<Value> {
        self.basic_block_data(bb).get_val(var)
    }

    pub fn assign_in_bb(&mut self, bb: BasicBlock, var: String, val: Value) {
        self.basic_block_data_mut(bb).assign(var, val);
    }

    pub fn make_new_basic_block(&mut self, edge: ControlFlowEdge) -> BasicBlock {
        self.push_basic_block(BasicBlockData::new(edge))
    }

    /// Base means basic block whose values should serve as the base for the new one
    /// Parent means parent in the domination hierarchy
    pub fn make_new_basic_block_from(
        &mut self,
        base: BasicBlock,
        parent: BasicBlock,
        edge: ControlFlowEdge
    ) -> BasicBlock {
        let bb = BasicBlockData::new_from(self.basic_block_data(base), edge, parent);
        self.push_basic_block(bb)
    }

    pub fn make_new_root(&mut self) -> BasicBlock {
        let edge = match self.root {
            Some(root) => ControlFlowEdge::Fallthrough(root),
            None => ControlFlowEdge::Leaf,
        };
        let new_root = self.make_new_basic_block(edge);

        if let ControlFlowEdge::Fallthrough(old_root) = edge {
            self.establish_dominance(new_root, old_root);
        }

        self.root = Some(new_root);
        new_root
    }

    fn push_basic_block(&mut self, bb: BasicBlockData) -> BasicBlock {
        self.blocks.push(bb);
        BasicBlock(self.blocks.len() - 1)
    }

    pub fn push_instr(&mut self, block: BasicBlock, instr: Instruction) {
        self.basic_block_data_mut(block).push_instr(instr);
    }

    pub fn set_edge_for_block(&mut self, bb: BasicBlock, edge: ControlFlowEdge) {
        self.basic_block_data_mut(bb).set_edge(edge);
    }

    pub fn connect_via_fallthrough(&mut self, src: BasicBlock, dest: BasicBlock) {
        self.set_edge_for_block(src, ControlFlowEdge::Fallthrough(dest))
    }

    pub fn connect_via_branch(&mut self, src: BasicBlock, dest: BasicBlock) {
        self.set_edge_for_block(src, ControlFlowEdge::Branch(dest));
        self.push_instr(src, Instruction::UnconditionalBranch(dest));
    }

    pub fn establish_dominance(&mut self, parent: BasicBlock, child: BasicBlock) {
        self.basic_block_data_mut(child).set_dominator(parent);
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    Bind(Value, CCLocation),
    Branch(BranchOpcode, Value, BasicBlock),
    Call(String),
    Const(i32, Value),
    End,
    Move(Value, CCLocation),
    Nop,
    Read(Value),
    Return,
    StoredBinaryOp(StoredBinaryOpcode, Value, Value, Value),
    UnconditionalBranch(BasicBlock),
    Write(Value),
    Writeln,
}

impl Instruction {
    pub fn result_val(&self) -> Option<Value> {
        match self {
            Instruction::Const(_, dest) => Some(*dest),
            Instruction::Read(dest) => Some(*dest),
            Instruction::StoredBinaryOp(_, _, _, dest) => Some(*dest),
            _ => None,
        }
    }

    // We could do some iterator magic here, but that would require an extra allocation anyway
    // because the iterator types are heterogeneous, so it's just easier to return a vector
    pub fn operands(&self) -> Vec<Value> {
        match self {
            Instruction::StoredBinaryOp(_, src1, src2, _) => vec![*src1, *src2],
            Instruction::Write(src) => vec![*src],
            Instruction::Branch(_, cmp, _) => vec![*cmp],
            _ => vec![],
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instruction::Call(func) => write!(f, "call {}", func),
            Instruction::Const(n, dest) => write!(f, "{} = const {}", dest, n),
            Instruction::Branch(opcode, cmp, dest) => write!(f, "{} {}, {}", opcode, cmp, dest),
            Instruction::StoredBinaryOp(opcode, src1, src2, dest) => write!(f, "{} = {} {}, {}", dest, opcode, src1, src2),
            Instruction::Read(dest) => write!(f, "{} = read", dest),
            Instruction::Return => write!(f, "ret"),
            Instruction::Write(src) => write!(f, "write {}", src),
            Instruction::Writeln => write!(f, "writeln"),
            Instruction::End => write!(f, "end"),
            Instruction::Nop => write!(f, "nop"),
            Instruction::Bind(val, loc) => write!(f, "bind {}, {}", val, loc),
            Instruction::Move(val, loc) => write!(f, "move {}, {}", val, loc),
            Instruction::UnconditionalBranch(dest) => write!(f, "br {}", dest),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BranchOpcode {
    Beq, Bne, Bgt, Bge, Blt, Ble,
}

impl Display for BranchOpcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
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

impl From<BranchOpcode> for F1Opcode {
    fn from(opcode: BranchOpcode) -> Self {
        match opcode {
            BranchOpcode::Beq => F1Opcode::Beq,
            BranchOpcode::Bne => F1Opcode::Bne,
            BranchOpcode::Bgt => F1Opcode::Bgt,
            BranchOpcode::Bge => F1Opcode::Bge,
            BranchOpcode::Blt => F1Opcode::Blt,
            BranchOpcode::Ble => F1Opcode::Ble,
        }
    }
}

impl TryFrom<StoredBinaryOpcode> for F1Opcode {
    type Error = StoredBinaryOpcode;

    fn try_from(opcode: StoredBinaryOpcode) -> Result<Self, Self::Error> {
        match opcode {
            StoredBinaryOpcode::Add => Ok(F1Opcode::Addi),
            StoredBinaryOpcode::Sub => Ok(F1Opcode::Subi),
            StoredBinaryOpcode::Mul => Ok(F1Opcode::Muli),
            StoredBinaryOpcode::Div => Ok(F1Opcode::Divi),
            StoredBinaryOpcode::Cmp => Ok(F1Opcode::Cmpi),
            opcode @ StoredBinaryOpcode::Phi => Err(opcode),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum StoredBinaryOpcode {
    Add, Sub, Mul, Div, Cmp, Phi,
}

impl Display for StoredBinaryOpcode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            StoredBinaryOpcode::Add => write!(f, "add"),
            StoredBinaryOpcode::Sub => write!(f, "sub"),
            StoredBinaryOpcode::Mul => write!(f, "mul"),
            StoredBinaryOpcode::Div => write!(f, "div"),
            StoredBinaryOpcode::Cmp => write!(f, "cmp"),
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

impl From<StoredBinaryOpcode> for F2Opcode {
    fn from(opcode: StoredBinaryOpcode) -> Self {
        match opcode {
            StoredBinaryOpcode::Add => F2Opcode::Add,
            StoredBinaryOpcode::Sub => F2Opcode::Sub,
            StoredBinaryOpcode::Mul => F2Opcode::Mul,
            StoredBinaryOpcode::Div => F2Opcode::Div,
            StoredBinaryOpcode::Cmp => F2Opcode::Cmp,
            StoredBinaryOpcode::Phi => todo!(),
        }
    }
}


// wrapper around interned index
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BasicBlock(pub usize);

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlockData {
    pub(super) body: Vec<Instruction>,
    pub(super) val_table: HashMap<String, Value>,
    pub(super) edge: ControlFlowEdge,
    pub(super) dominator: Option<BasicBlock>,
}

impl BasicBlockData {
    pub fn new(edge: ControlFlowEdge) -> BasicBlockData {
        BasicBlockData::from(edge, HashMap::new(), None)
    }

    pub fn new_from(bb: &BasicBlockData, edge: ControlFlowEdge, dominator: BasicBlock) -> BasicBlockData {
        BasicBlockData::from(edge, bb.val_table.clone(), Some(dominator))
    }

    #[cfg(test)]
    pub fn with(
        body: Vec<Instruction>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlock>,
        val_table: HashMap<String, Value>,
    ) -> BasicBlockData {
        BasicBlockData { body, edge, dominator, val_table }
    }

    fn from(edge: ControlFlowEdge, val_table: HashMap<String, Value>, dominator: Option<BasicBlock>) -> BasicBlockData {
        BasicBlockData {
            body: vec![],
            val_table,
            edge,
            dominator,
        }
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
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

    pub fn edge(&self) -> ControlFlowEdge {
        self.edge
    }

    pub fn set_edge(&mut self, edge: ControlFlowEdge) {
        self.edge = edge;
    }

    pub fn push_instr(&mut self, instr: Instruction) {
        self.body.push(instr);
    }

    pub fn dominator(&self) -> Option<BasicBlock> {
        self.dominator
    }

    pub fn set_dominator(&mut self, dominator: BasicBlock) {
        if self.dominator.replace(dominator).is_some() {
            panic!("tried to set basic block dominator twice");
        }
    }

    pub fn is_empty(&self) -> bool {
        self.body.is_empty()
    }

    pub fn phis(&self) -> impl Iterator<Item = (Value, Value, Value)> + '_ {
        self.body().iter().filter_map(|instr|
            match instr {
                Instruction::StoredBinaryOp(StoredBinaryOpcode::Phi, src1, src2, dest) => Some((*src1, *src2, *dest)),
                _ => None,
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlock),
    Branch(BasicBlock),
    IfStmt(BasicBlock, Option<BasicBlock>, BasicBlock),
    Loop(BasicBlock, BasicBlock),
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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


/// Represents a "calling convention location", a special storage location whose exact value is
/// dictated by architecture-specific calling conventions. This provides an architecture-agnostic
/// way to bind values to locations with mu instructions
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CCLocation {
    Arg(usize),
    RetVal,
}

impl Display for CCLocation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            CCLocation::Arg(n) => write!(f, "[ArgLoc{}]", n),
            CCLocation::RetVal => write!(f, "[RetValLoc]"),
        }
    }
}
