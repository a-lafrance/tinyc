use std::{
    collections::{hash_map::Iter as HashMapIter, HashMap},
    fmt::{self, Display, Formatter},
};
use crate::{
    ast::{FactorOp, TermOp},
    utils::RelOp,
};


#[derive(Debug)]
pub struct Body {
    blocks: Vec<BasicBlockData>,
    root: Option<BasicBlock>,
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
        self.root = Some(bb);
    }

    pub fn basic_block_data(&self, bb: BasicBlock) -> &BasicBlockData {
        &self.blocks[bb.0]
    }

    pub fn basic_block_data_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockData {
        &mut self.blocks[bb.0]
    }

    pub fn val_in_bb(&self, bb: BasicBlock, var: &str) -> Option<Value> {
        self.basic_block_data(bb).get_val(var)
    }

    pub fn assign_in_bb(&mut self, bb: BasicBlock, var: String, val: Value) {
        self.basic_block_data_mut(bb).assign(var, val);
    }

    pub fn make_new_basic_block(&mut self) -> BasicBlock {
        self.push_basic_block(BasicBlockData::new())
    }

    /// Base means basic block whose values should serve as the base for the new one
    /// Parent means parent in the domination hierarchy
    pub fn make_new_basic_block_from(&mut self, base: BasicBlock, parent: BasicBlock) -> BasicBlock {
        let bb = BasicBlockData::new_from(self.basic_block_data(base), parent);
        self.push_basic_block(bb)
    }

    fn push_basic_block(&mut self, bb: BasicBlockData) -> BasicBlock {
        self.blocks.push(bb);
        BasicBlock(self.blocks.len() - 1)
    }

    pub fn push_instr(&mut self, block: BasicBlock, instr: Instruction) {
        self.basic_block_data_mut(block).push_instr(instr);
    }

    pub fn connect_via_fallthrough(&mut self, src: BasicBlock, dest: BasicBlock) {
        self.basic_block_data_mut(src).set_fallthrough_dest(dest);
    }

    pub fn connect_via_branch(&mut self, src: BasicBlock, dest: BasicBlock, branch_type: BranchOpcode) {
        self.basic_block_data_mut(src).set_branch_dest(dest);
        self.push_instr(src, Instruction::Branch(branch_type, dest));
    }

    pub fn establish_dominance(&mut self, parent: BasicBlock, child: BasicBlock) {
        self.basic_block_data_mut(child).set_dominator(parent);
    }
}


#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    Branch(BranchOpcode, BasicBlock),
    Call(String),
    Const(u32, Value),
    Cmp(Value, Value),
    End,
    Mu(Value, CCLocation),
    Nop,
    Read(Value),
    Return,
    StoredBinaryOp { opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value },
    Write(Value),
    Writeln,
}

impl Instruction {
    pub fn result_val(&self) -> Option<Value> {
        match self {
            Instruction::Const(_, dest) => Some(*dest),
            Instruction::Read(dest) => Some(*dest),
            Instruction::StoredBinaryOp { dest, .. } => Some(*dest),
            Instruction::Mu(val, _) => Some(*val),
            _ => None,
        }
    }

    // We could do some iterator magic here, but that would require an extra allocation anyway
    // because the iterator types are heterogeneous, so it's just easier to return a vector
    pub fn operands(&self) -> Vec<Value> {
        match self {
            Instruction::Cmp(lhs, rhs) => vec![*lhs, *rhs],
            Instruction::StoredBinaryOp { src1, src2, .. } => vec![*src1, *src2],
            Instruction::Write(src) => vec![*src],
            Instruction::Mu(val, _) => vec![*val],
            _ => vec![],
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instruction::Call(func) => write!(f, "call {}", func),
            Instruction::Const(n, dest) => write!(f, "{} = const {}", dest, n),
            Instruction::Cmp(lhs, rhs) => write!(f, "cmp {}, {}", lhs, rhs),
            Instruction::Branch(opcode, dest) => write!(f, "{} {}", opcode, dest),
            Instruction::StoredBinaryOp { opcode, src1, src2, dest } => write!(f, "{} = {} {}, {}", dest, opcode, src1, src2),
            Instruction::Read(dest) => write!(f, "{} = read", dest),
            Instruction::Return => write!(f, "ret"),
            Instruction::Write(src) => write!(f, "write {}", src),
            Instruction::Writeln => write!(f, "writeln"),
            Instruction::End => write!(f, "end"),
            Instruction::Nop => write!(f, "nop"),
            Instruction::Mu(val, loc) => write!(f, "mu {}, {}", val, loc),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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
    dominator: Option<BasicBlock>,
}

impl BasicBlockData {
    pub fn new() -> BasicBlockData {
        BasicBlockData::from(HashMap::new(), None)
    }

    pub fn new_from(bb: &BasicBlockData, dominator: BasicBlock) -> BasicBlockData {
        BasicBlockData::from(bb.val_table.clone(), Some(dominator))
    }

    #[cfg(test)]
    pub fn with(
        body: Vec<Instruction>,
        fallthrough_dest: Option<BasicBlock>,
        branch_dest: Option<BasicBlock>,
        dominator: Option<BasicBlock>
    ) -> BasicBlockData {
        BasicBlockData { body, fallthrough_dest, branch_dest, dominator, val_table: HashMap::new() }
    }

    fn from(val_table: HashMap<String, Value>, dominator: Option<BasicBlock>) -> BasicBlockData {
        BasicBlockData {
            body: vec![],
            val_table,
            fallthrough_dest: None,
            branch_dest: None,
            dominator
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

    pub fn push_instr(&mut self, instr: Instruction) {
        self.body.push(instr);
    }

    pub fn fallthrough_dest(&self) -> Option<BasicBlock> {
        self.fallthrough_dest
    }

    pub fn set_fallthrough_dest(&mut self, dest: BasicBlock) {
        self.fallthrough_dest = Some(dest);
    }

    pub fn branch_dest(&self) -> Option<BasicBlock> {
        self.branch_dest
    }

    pub fn set_branch_dest(&mut self, dest: BasicBlock) {
        self.branch_dest = Some(dest);
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
                Instruction::StoredBinaryOp { opcode: StoredBinaryOpcode::Phi, src1, src2, dest } => Some((*src1, *src2, *dest)),
                _ => None,
            }
        )
    }

    pub fn control_flow_kind(&self, body: &Body) -> ControlFlowKind {
        if let Some(ft_dest) = self.fallthrough_dest() {
            if let Some(br_dest) = self.branch_dest() {
                ControlFlowKind::IfStmt(ft_dest, br_dest)
            } else {
                if let Some(loop_body) = body.basic_block_data(ft_dest).loop_body(ft_dest, body) {
                    ControlFlowKind::Loop(ft_dest, loop_body)
                } else {
                    ControlFlowKind::FallthroughOnly(ft_dest)
                }
            }
        } else if let Some(br_dest) = self.branch_dest() {
            ControlFlowKind::UnconditionalBranch(br_dest)
        } else {
            ControlFlowKind::Leaf
        }
    }

    fn loop_body(&self, self_bb: BasicBlock, body: &Body) -> Option<BasicBlock> {
        // if this block's ft dest is UnconditionalBranch with dest = self, then yes
        let ft_dest = self.fallthrough_dest()?;
        let ft_bb_data = body.basic_block_data(ft_dest);

        let ft_br_dest = ft_bb_data.branch_dest()?;

        if ft_bb_data.fallthrough_dest().is_none() && ft_br_dest == self_bb {
            Some(ft_dest)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ControlFlowKind {
    Leaf,
    FallthroughOnly(BasicBlock),
    UnconditionalBranch(BasicBlock),
    IfStmt(BasicBlock, BasicBlock),
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
