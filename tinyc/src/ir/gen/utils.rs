use std::collections::HashSet;
use bimap::BiBTreeMap;
use crate::{
    ast::{
        visit::AstVisitor,
        Assignment, Block,
    },
    ir::isa::{BasicBlockData, Body, Instruction, Value},
};

#[derive(Default)]
pub struct ConstAllocator {
    mapping: BiBTreeMap<u32, Value>,
}

impl ConstAllocator {
    pub fn is_empty(&self) -> bool {
        self.mapping.is_empty()
    }

    pub fn val_for_const(&self, n: u32) -> Option<Value> {
        self.mapping.get_by_left(&n).copied()
    }

    pub fn const_for_val(&self, val: Value) -> Option<u32> {
        self.mapping.get_by_right(&val).copied()
    }

    pub fn alloc(&mut self, n: u32, val: Value) {
        self.mapping.insert(n, val);
    }

    pub fn make_prelude_block(&self, body: &mut Body) {
        if !self.is_empty() {
            let prelude_block = body.make_new_root();

            for (n, val) in self.mapping.iter().map(|(n, v)| (*n, *v)) {
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
