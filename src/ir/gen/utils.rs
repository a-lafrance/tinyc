use std::collections::{HashMap, HashSet};
use crate::{
    ast::{
        visit::AstVisitor,
        Assignment, Block,
    },
    ir::isa::{BasicBlockData, Body, Instruction, Value},
};

pub struct ConstAllocator(HashMap<u32, Value>);

impl ConstAllocator {
    pub fn new() -> ConstAllocator {
        ConstAllocator(HashMap::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, n: u32) -> Option<Value> {
        self.0.get(&n).copied()
    }

    pub fn alloc(&mut self, n: u32, val: Value) {
        self.0.insert(n, val);
    }

    pub fn make_prelude_block(&self, body: &mut Body) {
        if !self.is_empty() {
            let prelude_block = body.make_new_basic_block();

            for (n, val) in self.0.iter().map(|(n, v)| (*n, *v)) {
                body.push_instr(prelude_block, Instruction::Const(n, val));
            }

            if let Some(root) = body.root_block() {
                body.connect_via_fallthrough(prelude_block, root);
                body.establish_dominance(prelude_block, root);
            }

            body.set_root_block(prelude_block);
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
