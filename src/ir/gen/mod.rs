mod utils;

use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop, Relation, Return, Term,
        visit::{self, AstVisitor},
    },
    utils::{Builtin, Keyword},
};
use self::utils::{ConstAllocator, PhiDetectionPass};
use super::{
    isa::{BasicBlock, Body, BranchOpcode, Instruction, StoredBinaryOpcode, Value},
    IrStore,
};


pub struct IrGenerator(IrStore);

impl IrGenerator {
    pub fn gen(ast: &Computation) -> IrStore {
        let mut gen = IrGenerator::new();
        gen.visit_computation(ast);
        gen.0
    }

    fn new() -> IrGenerator {
        IrGenerator(IrStore::new())
    }
}

impl AstVisitor for IrGenerator {
    fn visit_computation(&mut self, comp: &Computation) {
        for func in comp.funcs.iter() {
            self.visit_func_decl(func);
        }

        self.0.register(
            Keyword::Main.to_string(),
            IrBodyGenerator::gen_from_computation(comp),
        );
    }

    fn visit_func_decl(&mut self, func: &FuncDecl) {
        self.0.register(
            func.name.clone(),
            IrBodyGenerator::gen_from_func_decl(func),
        );
    }
}


struct IrBodyGenerator {
    body: Body,
    const_alloc: ConstAllocator,
    last_val: Option<Value>,
    next_val: Value,
    current_block: Option<BasicBlock>,
}

impl IrBodyGenerator {
    pub fn gen_from_computation(comp: &Computation) -> Body {
        let mut gen = IrBodyGenerator::new();
        gen.visit_computation(comp);
        gen.const_alloc.make_prelude_block(&mut gen.body);
        gen.body
    }

    pub fn gen_from_func_decl(func: &FuncDecl) -> Body {
        let mut gen = IrBodyGenerator::new();
        gen.visit_func_decl(func);
        gen.const_alloc.make_prelude_block(&mut gen.body);
        gen.body
    }

    fn new() -> IrBodyGenerator {
        IrBodyGenerator {
            body: Body::new(),
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
        self.last_val = self.current_block.map(|bb| self.body.basic_block_data(bb)).and_then(|bb| bb.get_val(var));
    }

    fn load_const(&mut self, n: u32) {
        self.last_val = Some(self.const_alloc.get(n).unwrap_or_else(|| {
            let val = self.alloc_val();
            self.const_alloc.alloc(n, val);

            val
        }));
    }

    fn fill_basic_block(&mut self) -> BasicBlock {
        let bb = self.body.make_new_basic_block();
        self.current_block = Some(bb);
        bb
    }

    fn fill_basic_block_from(&mut self, parent: BasicBlock) -> BasicBlock {
        let bb = self.body.make_new_basic_block_from(parent);
        self.current_block = Some(bb);
        bb
    }

    /// Detects differences between the values in bb1 and bb2, and generates the
    /// corresponding phi instructions in dest
    fn generate_phis(&mut self, bb1: BasicBlock, bb2: BasicBlock, dest: BasicBlock) -> Vec<(Value, Value, Value)>{
        let bb1_data = self.body.basic_block_data(bb1);
        let bb2_data = self.body.basic_block_data(bb2);
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
            self.body.push_instr(
                dest,
                Instruction::StoredBinaryOp { opcode: StoredBinaryOpcode::Phi, src1: val1, src2: val2, dest: phi_val }
            );

            // update join block val table
            self.body.basic_block_data_mut(dest).assign(var, phi_val);
            phis.push((val1, val2, phi_val));
        }

        phis
    }
}

impl AstVisitor for IrBodyGenerator {
    fn visit_assignment(&mut self, assign: &Assignment) {
        self.visit_expr(&assign.value);
        self.body.basic_block_data_mut(self.current_block.expect("invariant violated: can only assign in block"))
            .assign(
                assign.place.clone(),
                self.last_val.expect("invariant violated: assignment to non-expression"),
            );
    }

    fn visit_block(&mut self, block: &Block) {
        let bb = self.current_block.expect("invariant violated: basic block not created for block");

        if block.is_empty() {
            self.body.push_instr(bb, Instruction::Nop);
        } else {
            visit::walk_block(self, block);
        }
    }

    fn visit_computation(&mut self, comp: &Computation) {
        let main_block = self.fill_basic_block();
        self.visit_block(&comp.body);

        self.body.set_root_block(main_block);
        self.body.push_instr(
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
            self.body.push_instr(
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
            None => unimplemented!(), // TODO
        };

        self.body.push_instr(
            self.current_block.expect("invariant violated: func call must be in block"),
            instr
        );
    }

    fn visit_func_decl(&mut self, decl: &FuncDecl) {
        // TODO: pop params
        let root = self.fill_basic_block();
        visit::walk_func_decl(self, decl);

        self.body.set_root_block(root);
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
        self.body.connect_via_fallthrough(condition_bb, then_bb);

        // pre-allocate join basic block
        // use the then block as the basis for the join block's values to make phi discovery easier
        let join_bb = self.body.make_new_basic_block_from(then_end_bb);

        // connect inner blocks together depending on presence of else
        let (dest_bb, phi_compare_bb) = match if_stmt.else_block {
            Some(ref else_block) => {
                // if else block exists, fill new basic block for it
                let else_bb = self.fill_basic_block_from(condition_bb);
                self.visit_block(else_block);
                let else_end_bb = self.current_block.expect("invariant violated: else block must end in a bb");

                // connect then block to join block via branch
                // connect else block to join block via fallthrough
                self.body.connect_via_branch(then_end_bb, join_bb, BranchOpcode::Br);
                self.body.connect_via_fallthrough(else_end_bb, join_bb);
                (else_bb, else_end_bb)
            },

            None => {
                // connect then block to join block via fallthrough
                self.body.connect_via_fallthrough(then_end_bb, join_bb);
                (join_bb, condition_bb)
            },
        };

        // connect start block to destination block (either join or else) via conditional branch
        let branch_opcode = BranchOpcode::from(if_stmt.condition.op.negated());
        self.body.connect_via_branch(condition_bb, dest_bb, branch_opcode);

        // fast-forward to join block
        self.current_block = Some(join_bb);
        self.generate_phis(join_bb, phi_compare_bb, join_bb);
    }

    fn visit_loop(&mut self, loop_stmt: &Loop) {
        let prev_bb = self.current_block.expect("invariant violated: loop must be in block");
        let start_bb = self.body.make_new_basic_block_from(prev_bb);
        self.body.connect_via_fallthrough(prev_bb, start_bb);

        let phis = PhiDetectionPass::run(self.body.basic_block_data(start_bb), &loop_stmt.body)
            .into_iter()
            .map(|var| {
                let old_val = self.body.val_in_bb(start_bb, &var).expect("invariant violated: phi detection returned non-existent var");
                let dest_val = self.alloc_val();
                self.body.assign_in_bb(start_bb, var.clone(), dest_val);

                (var, Instruction::StoredBinaryOp {
                    opcode: StoredBinaryOpcode::Phi,
                    src1: old_val,
                    src2: Value(0),
                    dest: dest_val
                })
            })
            .collect::<Vec<_>>();

        let body_bb = self.fill_basic_block_from(start_bb);
        self.body.connect_via_fallthrough(start_bb, body_bb);
        self.visit_block(&loop_stmt.body);

        self.body.connect_via_branch(body_bb, start_bb, BranchOpcode::Br);
        let post_bb = self.body.make_new_basic_block_from(start_bb);

        self.current_block = Some(start_bb);

        for (var, mut phi) in phis.into_iter() {
            match phi {
                Instruction::StoredBinaryOp { ref mut src2, .. } => *src2 = self.body
                    .val_in_bb(body_bb, &var)
                    .expect("invariant violated: val not found for var in phi instruction"),
                _ => unreachable!(),
            }

            self.body.push_instr(start_bb, phi);
        }

        self.visit_relation(&loop_stmt.condition);
        self.body.connect_via_branch(start_bb, post_bb, BranchOpcode::from(loop_stmt.condition.op.negated()));
        self.current_block = Some(post_bb);
    }

    fn visit_relation(&mut self, relation: &Relation) {
        self.visit_expr(&relation.lhs);
        let lhs = self.last_val.expect("invariant violated: expected expr");

        self.visit_expr(&relation.rhs);
        let rhs = self.last_val.expect("invariant violated: expected expr");

        self.body.push_instr(
            self.current_block.expect("invariant violated: func call must be in block"),
            Instruction::Cmp(lhs, rhs)
        );
    }

    /*
    function add(x, y);
    {
        return x + y;
    };

    */
    fn visit_return(&mut self, ret: &Return) {
        visit::walk_return(self, ret);

        self.body.push_instr(
            self.current_block.expect("invariant violated: return must be in block"),
            Instruction::Return(self.last_val),
        );
    }

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
            self.body.push_instr(
                self.current_block.expect("invariant violated: term must be in block"),
                instr
            );
        }
    }
}
