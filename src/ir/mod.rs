mod gen;
pub mod isa;
pub mod visit;

use crate::ast::Computation;
use self::{
    gen::IrGenerator,
    isa::{BasicBlock, BasicBlockData, BranchOpcode, InstrData, Instruction, Value},
};

#[derive(Debug)]
pub struct IrStore {
    instrs: Vec<InstrData>,
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

    pub fn val_in_bb(&self, bb: BasicBlock, var: &str) -> Option<Value> {
        self.basic_block_data(bb).get_val(var)
    }

    pub fn assign_in_bb(&mut self, bb: BasicBlock, var: String, val: Value) {
        self.basic_block_data_mut(bb).assign(var, val);
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

    pub fn instr(&self, i: Instruction) -> InstrData {
        self.instrs[i.0]
    }

    pub fn push_instr(&mut self, block: BasicBlock, instr_data: InstrData) -> Instruction {
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
        self.push_instr(src, InstrData::Branch(branch_type, dest));
    }
}

impl From<Computation> for IrStore {
    fn from(ast: Computation) -> IrStore {
        IrGenerator::gen(&ast)
    }
}
