use std::collections::HashMap;
use crate::{
    ast::{FactorOp, TermOp},
    ir::isa::{BasicBlock, Body, Value},
};

pub struct CseCache(HashMap<BasicBlock, InstrIndex>);

impl CseCache {
    pub fn new() -> Self {
        CseCache(HashMap::new())
    }

    pub fn insert_block(&mut self, bb: BasicBlock) {
        self.0.insert(bb, InstrIndex::new());
    }

    pub fn get_common_subexpr(&self, body: &Body, bb: BasicBlock, instr: &IndexableInstr) -> Option<Value> {
        let mut current_bb = Some(bb);

        while let (Some(bb), Some(index)) = (current_bb, current_bb.and_then(|bb| self.0.get(&bb))) {
            // check current block instr index
            // if present, return dest of existing instr
            if let Some(existing_value) = index.existing_value(instr) {
                return Some(existing_value);
            }

            // else, repeat with parent until you find or no parent left
            current_bb = body.basic_block_data(bb).dominator();
        }

        None
    }

    pub fn insert_instr(&mut self, bb: BasicBlock, instr: IndexableInstr, val: Value) {
        self.0.get_mut(&bb).expect("no index found for basic block")
            .insert(instr, val)
    }
}


#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IndexableInstr {
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Cmp(Value, Value),
    Call(String),
}

impl IndexableInstr {
    pub fn from_term_op(op: TermOp, lhs: Value, rhs: Value) -> Self {
        match op {
            TermOp::Add => IndexableInstr::Add(lhs, rhs),
            TermOp::Sub => IndexableInstr::Sub(lhs, rhs),
        }
    }

    pub fn from_factor_op(op: FactorOp, lhs: Value, rhs: Value) -> Self {
        match op {
            FactorOp::Mul => IndexableInstr::Mul(lhs, rhs),
            FactorOp::Div => IndexableInstr::Div(lhs, rhs),
        }
    }
}


struct InstrIndex(HashMap<IndexableInstr, Value>);

impl InstrIndex {
    pub fn new() -> InstrIndex {
        InstrIndex(HashMap::new())
    }

    pub fn existing_value(&self, instr: &IndexableInstr) -> Option<Value> {
        self.0.get(instr).copied()
    }

    pub fn insert(&mut self, instr: IndexableInstr, dest: Value) {
        self.0.insert(instr, dest);
    }
}
