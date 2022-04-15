use std::collections::{HashMap, HashSet};
use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop, Relation, Term,
        visit::{self, AstVisitor},
    },
    utils::Builtin,
};
use super::{
    isa::{BasicBlock, BasicBlockData, BranchOpcode, Instruction, StoredBinaryOpcode, Value},
    IrStore,
};

pub struct IrGenerator {
    store: IrStore,
    const_alloc: ConstAllocator,
    last_val: Option<Value>,
    next_val: Value,
    current_block: Option<BasicBlock>,
}

impl IrGenerator {
    pub fn gen(ast: &Computation) -> IrStore {
        let mut gen = IrGenerator::new();
        gen.visit_computation(ast);
        gen.const_alloc.make_prelude_block(&mut gen.store);
        gen.store
    }

    fn new() -> IrGenerator {
        IrGenerator {
            store: IrStore::new(),
            const_alloc: ConstAllocator::new(),
            last_val: None,
            next_val: Value(0),
            current_block: None,
        }
    }

    pub(self) fn alloc_val(&mut self) -> Value {
        let val = self.next_val;
        self.next_val.0 += 1;

        val
    }

    fn load_var(&mut self, var: &str) {
        self.last_val = self.current_block.map(|bb| self.store.basic_block_data(bb)).and_then(|bb| bb.get_val(var));
    }

    fn load_const(&mut self, n: u32) {
        self.last_val = Some(self.const_alloc.get(n).unwrap_or_else(|| {
            let val = self.alloc_val();
            self.const_alloc.alloc(n, val);

            val
        }));
    }

    fn fill_basic_block(&mut self) -> BasicBlock {
        let bb = self.store.make_new_basic_block();
        self.current_block = Some(bb);
        bb
    }

    fn fill_basic_block_from(&mut self, parent: BasicBlock) -> BasicBlock {
        let bb = self.store.make_new_basic_block_from(parent);
        self.current_block = Some(bb);
        bb
    }

    /// Detects differences between the values in bb1 and bb2, and generates the
    /// corresponding phi instructions in dest
    fn generate_phis(&mut self, bb1: BasicBlock, bb2: BasicBlock, dest: BasicBlock) -> Vec<(Value, Value, Value)>{
        let bb1_data = self.store.basic_block_data(bb1);
        let bb2_data = self.store.basic_block_data(bb2);
        let mismatches = bb1_data.values().filter_map(|(var, val)| {
            let val = *val;
            let other_val = bb2_data.get_val(var).expect("invariant violated: var not found");

            if val != other_val {
                Some((var.to_string(), val, other_val))
            } else {
                None
            }
        }).collect::<Vec<_>>();

        let mut phis = vec![];

        for (var, val1, val2) in mismatches.into_iter() {
            // gen phi instr
            let phi_val = self.alloc_val();
            self.store.push_instr(
                dest,
                Instruction::StoredBinaryOp { opcode: StoredBinaryOpcode::Phi, src1: val1, src2: val2, dest: phi_val }
            );

            // update join block val table
            self.store.basic_block_data_mut(dest).assign(var, phi_val);
            phis.push((val1, val2, phi_val));
        }

        phis
    }
}

impl AstVisitor for IrGenerator {
    fn visit_assignment(&mut self, assign: &Assignment) {
        self.visit_expr(&assign.value);
        self.store.basic_block_data_mut(self.current_block.expect("invariant violated: can only assign in block"))
            .assign(
                assign.place.clone(),
                self.last_val.expect("invariant violated: assignment to non-expression"),
            );
    }

    fn visit_block(&mut self, block: &Block) {
        let bb = self.current_block.expect("invariant violated: basic block not created for block");

        if block.is_empty() {
            self.store.push_instr(bb, Instruction::Nop);
        } else {
            visit::walk_block(self, block);
        }
    }

    fn visit_computation(&mut self, comp: &Computation) {
        let main_block = self.fill_basic_block();
        self.visit_block(&comp.body);

        self.store.set_root_block(main_block);
        self.store.push_instr(
            self.current_block.expect("invariant violated: expected block"),
            Instruction::End
        );
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.visit_term(&expr.root);

        for (op, term) in expr.ops.iter() {
            let lhs = self.last_val.expect("invariant violated: expected expr");
            self.visit_term(term);
            let rhs = self.last_val.expect("invariant violated: expected expr");
            let result = self.alloc_val();
            let instr = Instruction::StoredBinaryOp {
                opcode: StoredBinaryOpcode::from(*op),
                src1: lhs,
                src2: rhs,
                dest: result,
            };

            self.last_val = Some(result);
            self.store.push_instr(
                self.current_block.expect("invariant violated: expr must be in block"),
                instr
            );
        }
    }

    fn visit_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::VarRef(var) => self.load_var(var),
            Factor::Number(n) => self.load_const(*n),
            _ => visit::walk_factor(self, factor),
        }
    }

    fn visit_func_call(&mut self, call: &FuncCall) {
        let instr = match Builtin::from(&call.name) {
            Some(Builtin::InputNum) => {
                let val = self.alloc_val();
                self.last_val = Some(val);

                Instruction::Read(val)
            },
            Some(Builtin::OutputNum) => {
                self.visit_expr(&call.args[0]); // FIXME: a bit unsafe
                Instruction::Write(self.last_val.expect("invariant violated: expected expr"))
            },
            Some(Builtin::OutputNewLine) => Instruction::Writeln,
            None => unimplemented!(),
        };

        self.store.push_instr(
            self.current_block.expect("invariant violated: func call must be in block"),
            instr
        );
    }

    fn visit_func_decl(&mut self, _decl: &FuncDecl) {
        // FIXME: ignore for now
        // walk_func_decl(self, decl);
    }

    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) {
        // check condition in start basic block
        self.visit_relation(&if_stmt.condition);
        let condition_bb = self.current_block.expect("invariant violated: no basic block for if statement condition");

        // fill new basic block for then
        let then_bb = self.fill_basic_block_from(condition_bb);
        self.visit_block(&if_stmt.then_block);
        let then_end_bb = self.current_block.expect("invariant violated: then block must end in a bb");

        // connect start basic block to then basic block via fallthrough
        self.store.connect_via_fallthrough(condition_bb, then_bb);

        // pre-allocate join basic block
        // use the then block as the basis for the join block's values to make phi discovery easier
        let join_bb = self.store.make_new_basic_block_from(then_end_bb);

        // connect inner blocks together depending on presence of else
        let (dest_bb, phi_compare_bb) = match if_stmt.else_block {
            Some(ref else_block) => {
                // if else block exists, fill new basic block for it
                let else_bb = self.fill_basic_block_from(condition_bb);
                self.visit_block(else_block);
                let else_end_bb = self.current_block.expect("invariant violated: else block must end in a bb");

                // connect then block to join block via branch
                // connect else block to join block via fallthrough
                self.store.connect_via_branch(then_end_bb, join_bb, BranchOpcode::Br);
                self.store.connect_via_fallthrough(else_end_bb, join_bb);
                (else_bb, else_end_bb)
            },

            None => {
                // connect then block to join block via fallthrough
                self.store.connect_via_fallthrough(then_end_bb, join_bb);
                (join_bb, condition_bb)
            },
        };

        // connect start block to destination block (either join or else) via conditional branch
        let branch_opcode = BranchOpcode::from(if_stmt.condition.op.negated());
        self.store.connect_via_branch(condition_bb, dest_bb, branch_opcode);

        // fast-forward to join block
        self.current_block = Some(join_bb);
        self.generate_phis(join_bb, phi_compare_bb, join_bb);
    }

    fn visit_loop(&mut self, loop_stmt: &Loop) {
        let prev_bb = self.current_block.expect("invariant violated: loop must be in block");
        let start_bb = self.store.make_new_basic_block_from(prev_bb);
        self.store.connect_via_fallthrough(prev_bb, start_bb);

        let phis = PhiDetectionPass::run(self.store.basic_block_data(start_bb), &loop_stmt.body)
            .into_iter()
            .map(|var| {
                let old_val = self.store.val_in_bb(start_bb, &var).expect("invariant violated: phi detection returned non-existent var");
                let dest_val = self.alloc_val();
                self.store.assign_in_bb(start_bb, var.clone(), dest_val);

                (var, Instruction::StoredBinaryOp {
                    opcode: StoredBinaryOpcode::Phi,
                    src1: old_val,
                    src2: Value(0),
                    dest: dest_val
                })
            })
            .collect::<Vec<_>>();

        let body_bb = self.fill_basic_block_from(start_bb);
        self.store.connect_via_fallthrough(start_bb, body_bb);
        self.visit_block(&loop_stmt.body);

        self.store.connect_via_branch(body_bb, start_bb, BranchOpcode::Br);
        let post_bb = self.store.make_new_basic_block_from(start_bb);

        self.current_block = Some(start_bb);

        for (var, mut phi) in phis.into_iter() {
            match phi {
                Instruction::StoredBinaryOp { ref mut src2, .. } => *src2 = self.store
                    .val_in_bb(body_bb, &var)
                    .expect("invariant violated: val not found for var in phi instruction"),
                _ => unreachable!(),
            }

            self.store.push_instr(start_bb, phi);
        }

        self.visit_relation(&loop_stmt.condition);
        self.store.connect_via_branch(start_bb, post_bb, BranchOpcode::from(loop_stmt.condition.op.negated()));
        self.current_block = Some(post_bb);
    }

    fn visit_relation(&mut self, relation: &Relation) {
        self.visit_expr(&relation.lhs);
        let lhs = self.last_val.expect("invariant violated: expected expr");

        self.visit_expr(&relation.rhs);
        let rhs = self.last_val.expect("invariant violated: expected expr");

        self.store.push_instr(
            self.current_block.expect("invariant violated: func call must be in block"),
            Instruction::Cmp(lhs, rhs)
        );
    }

    // fn visit_return(&mut self, ret: &Return) {
    //     walk_return(self, ret);
    // }

    fn visit_term(&mut self, term: &Term) {
        self.visit_factor(&term.root);

        for (op, factor) in term.ops.iter() {
            let lhs = self.last_val.expect("invariant violated: expected expr");
            self.visit_factor(factor);
            let rhs = self.last_val.expect("invariant violated: expected expr");
            let result = self.alloc_val();
            let instr = Instruction::StoredBinaryOp {
                opcode: StoredBinaryOpcode::from(*op),
                src1: lhs,
                src2: rhs,
                dest: result,
            };

            self.last_val = Some(result);
            self.store.push_instr(
                self.current_block.expect("invariant violated: term must be in block"),
                instr
            );
        }
    }
}


struct ConstAllocator(HashMap<u32, Value>);

impl ConstAllocator {
    pub fn new() -> ConstAllocator {
        ConstAllocator(HashMap::new())
    }

    pub fn get(&self, n: u32) -> Option<Value> {
        self.0.get(&n).copied()
    }

    pub fn alloc(&mut self, n: u32, val: Value) {
        self.0.insert(n, val);
    }

    pub fn make_prelude_block(&self, store: &mut IrStore) {
        let prelude_block = store.make_new_basic_block();

        for (n, val) in self.0.iter().map(|(n, v)| (*n, *v)) {
            store.push_instr(prelude_block, Instruction::Const(n, val));
        }

        if let Some(root) = store.root_block() {
            store.connect_via_fallthrough(prelude_block, root);
        }

        store.set_root_block(prelude_block);
    }
}


struct PhiDetectionPass<'bb> {
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
