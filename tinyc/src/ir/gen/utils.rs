use std::collections::{HashMap, HashSet};
use crate::{
    ast::{
        visit::AstVisitor,
        Assignment, Block,
    },
    ir::isa::{BasicBlockData, Body, Instruction, Value},
};

#[derive(Default)]
pub struct ConstAllocator {
    by_const: HashMap<u32, Value>,
    by_val: HashMap<Value, u32>,
}

impl ConstAllocator {
    pub fn is_empty(&self) -> bool {
        self.by_val.is_empty()
    }

    pub fn val_for_const(&self, n: u32) -> Option<Value> {
        self.by_const.get(&n).copied()
    }

    pub fn const_for_val(&self, val: Value) -> Option<u32> {
        self.by_val.get(&val).copied()
    }

    pub fn alloc(&mut self, n: u32, val: Value) {
        self.by_const.insert(n, val);
        self.by_val.insert(val, n);
    }

    pub fn make_prelude_block(&self, body: &mut Body) {
        if !self.is_empty() {
            let prelude_block = body.make_new_root();

            for (n, val) in self.by_const.iter().map(|(n, v)| (*n, *v)) {
                body.push_instr(prelude_block, Instruction::Const(n, val));
            }
        }
    }
}


pub struct PhiDetectionPass<'bb> {
    bb: &'bb BasicBlockData,
    phis: HashSet<String>,
}

impl<'bb> PhiDetectionPass<'bb> {
    pub fn run(bb: &'bb BasicBlockData, block: &Block) -> HashSet<String> {
        let mut pass = PhiDetectionPass::new(bb);
        pass.visit_block(block);

        pass.phis
    }

    fn new(bb: &'bb BasicBlockData) -> PhiDetectionPass {
        PhiDetectionPass {
            bb,
            phis: HashSet::new(),
        }
    }
}

impl AstVisitor for PhiDetectionPass<'_> {
    fn visit_assignment(&mut self, assign: &Assignment) {
        if self.bb.get_val(&assign.place).is_some() {
            // NOTE: clone usually not good
            self.phis.insert(assign.place.clone());
        }
    }
}
