pub mod fmt;
mod gen;
pub mod isa;
pub mod visit;

use crate::ast::Computation;
use self::{
    gen::IrGenerator,
    isa::{BasicBlock, BasicBlockData, BranchOpcode, Instruction, Value},
};

#[derive(Debug)]
pub struct IrStore {
    blocks: Vec<BasicBlockData>,
    root: Option<BasicBlock>,
}
