mod cse;
mod utils;

use std::cmp::Ordering;
use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop, Relation, Return, Stmt, Term,
        visit::{self, AstVisitor},
    },
    driver::opt::OptConfig,
    utils::{Builtin, Keyword, RelOp},
};
use self::{
    cse::{CseCache, IndexableInstr},
    utils::{ConstAllocator, PhiDetectionPass},
};
use super::{
    isa::{BasicBlock, Body, BranchOpcode, CCLocation, ControlFlowEdge, Instruction, StoredBinaryOpcode, Value},
    IrStore,
};


pub struct IrGenerator {
    store: IrStore,
    opt: OptConfig,
}

impl IrGenerator {
    pub fn gen(ast: &Computation, opt: OptConfig) -> IrStore {
        let mut gen = IrGenerator::new(opt);
        gen.visit_computation(ast);
        gen.store
    }

    fn new(opt: OptConfig) -> IrGenerator {
        IrGenerator { store: IrStore::new(), opt }
    }
}

impl AstVisitor for IrGenerator {
    fn visit_computation(&mut self, comp: &Computation) {
        for func in comp.funcs.iter() {
            self.visit_func_decl(func);
        }

        self.store.register(
            Keyword::Main.to_string(),
            IrBodyGenerator::gen_from_computation(comp, self.opt),
        );
    }

    fn visit_func_decl(&mut self, func: &FuncDecl) {
        self.store.register(
            func.name.clone(),
            IrBodyGenerator::gen_from_func_decl(func, self.opt),
        );
    }
}


pub struct IrBodyGenerator {
    body: Body,
    const_alloc: ConstAllocator,
    last_val: Option<Value>,
    next_val: Value,
    cse_cache: Option<CseCache>,
    current_block: Option<BasicBlock>,
    current_block_returns: bool,
    opt: OptConfig,
}

impl IrBodyGenerator {
    pub fn gen_from_computation(comp: &Computation, opt: OptConfig) -> Body {
        let mut gen = IrBodyGenerator::new(opt);
        gen.visit_computation(comp);
        gen.const_alloc.make_prelude_block(&mut gen.body);
        gen.body
    }

    pub fn gen_from_func_decl(func: &FuncDecl, opt: OptConfig) -> Body {
        let mut gen = IrBodyGenerator::new(opt);
        gen.visit_func_decl(func);
        gen.const_alloc.make_prelude_block(&mut gen.body);
        gen.body
    }

    fn new(opt: OptConfig) -> IrBodyGenerator {
        let cse_cache = if opt.cse { Some(CseCache::new()) } else { None };

        IrBodyGenerator {
            body: Body::new(),
            const_alloc: ConstAllocator::default(),
            last_val: None,
            next_val: Value(0),
            cse_cache,
            current_block: None,
            current_block_returns: false,
            opt,
        }
    }

    #[cfg(test)]
    pub(self) fn into_body(mut self) -> Body {
        self.const_alloc.make_prelude_block(&mut self.body);
        self.body
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
        self.last_val = Some(self.const_alloc.val_for_const(n).unwrap_or_else(|| {
            let val = self.alloc_val();
            self.const_alloc.alloc(n, val);

            val
        }));
    }

    fn make_basic_block(&mut self) -> BasicBlock {
        let bb = self.body.make_new_basic_block(ControlFlowEdge::Leaf);

        if let Some(ref mut cse_cache) = self.cse_cache {
            cse_cache.insert_block(bb);
        }

        bb
    }

    fn make_basic_block_from(&mut self, base: BasicBlock, parent: BasicBlock) -> BasicBlock {
        let bb = self.body.make_new_basic_block_from(base, parent, ControlFlowEdge::Leaf);

        if let Some(ref mut cse_cache) = self.cse_cache {
            cse_cache.insert_block(bb);
        }

        bb
    }

    fn fill_basic_block(&mut self) -> BasicBlock {
        let bb = self.make_basic_block();
        self.current_block = Some(bb);
        bb
    }

    fn fill_basic_block_from(
        &mut self,
        base: BasicBlock,
        parent: BasicBlock,
    ) -> BasicBlock {
        let bb = self.make_basic_block_from(base, parent);
        self.current_block = Some(bb);
        bb
    }

    /// Detects differences between the values in bb1 and bb2, and generates the
    /// corresponding phi instructions in dest
    fn generate_phis(&mut self, bb1: BasicBlock, bb2: BasicBlock, dest: BasicBlock) -> Vec<(Value, Value, Value)> {
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
                Instruction::StoredBinaryOp(StoredBinaryOpcode::Phi, val1, val2, phi_val),
            );

            // update join block val table
            self.body.basic_block_data_mut(dest).assign(var, phi_val);
            phis.push((val1, val2, phi_val));
        }

        phis
    }

    fn try_const_compute(&self, op: StoredBinaryOpcode, v1: Value, v2: Value) -> Option<u32> {
        if self.opt.const_prop {
            // Special case if the values aren't constant, but you're comparing a value to itself
            if let StoredBinaryOpcode::Cmp = op {
                if v1 == v2 {
                    return Some(0);
                }
            }

            let c1 = self.const_alloc.const_for_val(v1)?;
            let c2 = self.const_alloc.const_for_val(v2)?;

            match op {
                StoredBinaryOpcode::Add => Some(c1 + c2),
                StoredBinaryOpcode::Sub => Some(c1 - c2), // BIG FIXME: THIS BREAKS FOR NEGATIVE NUMBERS
                StoredBinaryOpcode::Mul => Some(c1 * c2),
                StoredBinaryOpcode::Div => Some(c1 / c2),
                StoredBinaryOpcode::Cmp => Some(self.compute_const_cmp(c1, c2)),
                StoredBinaryOpcode::Phi => None,
            }
        } else {
            // Always fail the const computation if const prop is disabled
            None
        }
    }

    // Returns the result of the conditional branch if it can be evaluated at compile time
    fn try_const_conditional_branch(&self, op: RelOp, cmp_val: Value) -> Option<bool> {
        if self.opt.const_prop {
            let cmp = self.const_alloc.const_for_val(cmp_val)?;

            Some(match op {
                RelOp::Eq => cmp == 0,
                RelOp::Ne => cmp != 0,
                RelOp::Gt => cmp == 2,
                RelOp::Lt => cmp == 1,
                RelOp::Ge => cmp != 1,
                RelOp::Le => cmp != 2,
            })
        } else {
            None
        }
    }

    fn compute_const_cmp(&self, lhs: u32, rhs: u32) -> u32 {
        match lhs.cmp(&rhs) {
            Ordering::Equal => 0,
            Ordering::Less => 1,
            Ordering::Greater => 2,
        }
    }
}

impl AstVisitor for IrBodyGenerator {
    fn visit_assignment(&mut self, assign: &Assignment) {
        self.visit_expr(&assign.value);
        self.body.assign_in_bb(
            self.current_block.expect("invariant violated: can only assign in block"),
            assign.place.clone(),
            self.last_val.expect("invariant violated: assignment to non-expression"),
        );
    }

    fn visit_block(&mut self, block: &Block) {
        let bb = self.current_block.expect("invariant violated: basic block not created for block");
        visit::walk_block(self, block);

        if self.body.basic_block_data(bb).is_empty() {
            // FIXME: not triggering for some reason? specifically w/ dead code elimination
            self.body.push_instr(bb, Instruction::Nop);
        }
    }

    // test cases:
        // sanity check
        // complex program sanity check
        // multiple bb program -> end instr buffered in last block
    fn visit_computation(&mut self, comp: &Computation) {
        let main_block = self.fill_basic_block();
        self.visit_block(&comp.body);

        self.body.set_root_block(main_block);
        self.body.push_instr(
            self.current_block.expect("invariant violated: expected block"),
            Instruction::End
        );
    }

    // test cases:
        // just one term
        // single term op
        // many term ops
    fn visit_expr(&mut self, expr: &Expr) {
        self.visit_term(&expr.root);

        for (op, term) in expr.ops.iter() {
            let lhs = self.last_val.expect("invariant violated: expected expr");
            self.visit_term(term);
            let rhs = self.last_val.expect("invariant violated: expected expr");
            let opcode = StoredBinaryOpcode::from(*op);

            match self.try_const_compute(opcode, lhs, rhs) {
                Some(result) => self.load_const(result),
                None => {
                    let block = self.current_block.expect("invariant violated: expr must be in block");
                    let cse_index = IndexableInstr::from_term_op(*op, lhs, rhs);

                    self.last_val = self.cse_cache.as_ref()
                        .and_then(|c| c.get_common_subexpr(&self.body, block, &cse_index))
                        .or_else(|| {
                            let result = self.alloc_val();
                            let instr = Instruction::StoredBinaryOp(opcode, lhs, rhs, result);

                            if let Some(ref mut cse_cache) = self.cse_cache {
                                cse_cache.insert_instr(block, cse_index, result);
                            }

                            self.body.push_instr(block, instr);
                            Some(result)
                        });
                },
            }
        }
    }

    // test cases:
        // sanity check for each kind of factor
    fn visit_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::VarRef(var) => self.load_var(var),
            Factor::Number(n) => self.load_const(*n),
            f => visit::walk_factor(self, f),
        }
    }

    // test cases:
        // sanity check for each io builtin
        // no arg function
        // single arg function
        // many arg function
    fn visit_func_call(&mut self, call: &FuncCall) {
        let block = self.current_block.expect("invariant violated: func call must be in block");

        match Builtin::from(&call.name) {
            Some(Builtin::InputNum) => {
                let val = self.alloc_val();
                self.last_val = Some(val);
                self.body.push_instr(block, Instruction::Read(val));
            },
            Some(Builtin::OutputNum) => {
                self.visit_expr(&call.args[0]); // FIXME: a bit unsafe
                self.body.push_instr(
                    block,
                    Instruction::Write(self.last_val.expect("invariant violated: expected expr"))
                );
            },
            Some(Builtin::OutputNewLine) => self.body.push_instr(block, Instruction::Writeln),
            None => {
                // _can't_ cse calls because they may have side effects
                    // a more robust optimization could track which functions do and don't have side effects but eh
                // push all args
                let block = self.current_block.expect("invariant violated: must be in block");

                for (i, arg) in call.args.iter().enumerate() {
                    self.visit_expr(arg);
                    self.body.push_instr(
                        block,
                        Instruction::Move(
                            self.last_val.expect("invariant violated: arg must have val"),
                            CCLocation::Arg(i),
                        ),
                    );
                }

                let dest_val = self.alloc_val();
                self.body.push_instr(block, Instruction::Call(call.name.clone()));
                self.body.push_instr(block, Instruction::Bind(dest_val, CCLocation::RetVal));

                self.last_val = Some(dest_val);
            },
        }
    }

    // test cases:
        // no param function
        // single param function
        // many param function
        // complex function sanity check
    fn visit_func_decl(&mut self, decl: &FuncDecl) {
        let root = self.fill_basic_block();
        self.body.set_root_block(root);

        for (i, param) in decl.params.iter().cloned().enumerate() {
            let param_val = self.alloc_val();
            self.body.assign_in_bb(root, param, param_val);
            self.body.push_instr(root, Instruction::Bind(param_val, CCLocation::Arg(i)));
        }

        visit::walk_func_decl(self, decl);
    }

    // test cases:
        // sanity check with else
            // with phis
            // no phis
        // sanity check no else
            // with phis
            // no phis
        // complex control flow (every then/else config)
            // complex then branch
                // if statement
                // loop
            // complex else branch
                // if statement
                // loop
            // even deeper nesting?
    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) {
        // check condition in start basic block
        self.visit_relation(&if_stmt.condition);
        let condition_val = self.last_val.expect("invariant violated: expected value for if statement condition");
        let condition_bb = self.current_block.expect("invariant violated: no basic block for if statement condition");

        match self.try_const_conditional_branch(if_stmt.condition.op, condition_val) {
            // If you can evaluate the branch at compile time, directly visit that block and continue
            Some(true) => self.visit_block(&if_stmt.then_block),
            Some(false) => if let Some(ref else_block) = if_stmt.else_block {
                self.visit_block(else_block);
            }

            None => {
                // fill new basic block for then
                let then_bb = self.fill_basic_block_from(condition_bb, condition_bb);
                self.visit_block(&if_stmt.then_block);
                let then_end_bb = self.current_block.expect("invariant violated: then block must end in a bb");

                // reset return status after saving status for then path
                let mut both_paths_return = self.current_block_returns;
                self.current_block_returns = false;

                // pre-allocate join basic block
                // use the then block as the basis for the join block's values to make phi discovery easier
                let join_bb = self.make_basic_block_from(then_end_bb, condition_bb);

                // connect inner blocks together depending on presence of else
                let (dest_bb, phi_compare_bb, else_bb) = match if_stmt.else_block {
                    Some(ref else_block) => {
                        // if else block exists, fill new basic block for it
                        let else_bb = self.fill_basic_block_from(condition_bb, condition_bb);
                        self.visit_block(else_block);
                        let else_end_bb = self.current_block.expect("invariant violated: else block must end in a bb");
                        both_paths_return = both_paths_return && self.current_block_returns;

                        // connect then block to join block via branch
                        // connect else block to join block via fallthrough
                        // FIXME: this produces an extra branch instruction when the then path returns
                        self.body.connect_via_branch(then_end_bb, join_bb);
                        self.body.connect_via_fallthrough(else_end_bb, join_bb);
                        (else_bb, else_end_bb, Some(else_bb))
                    },

                    None => {
                        // connect then block to join block via fallthrough
                        both_paths_return = false; // "both paths return" implies there are 2 paths
                        self.body.connect_via_fallthrough(then_end_bb, join_bb);
                        (join_bb, condition_bb, None)
                    },
                };

                // connect start block to destination block (either join or else) via conditional branch
                let branch_opcode = BranchOpcode::from(if_stmt.condition.op.negated());
                self.body.set_edge_for_block(condition_bb, ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb));
                self.body.push_instr(condition_bb, Instruction::Branch(branch_opcode, condition_val, dest_bb));

                // fast-forward to join block
                self.current_block = Some(join_bb);
                self.current_block_returns = both_paths_return;
                self.generate_phis(join_bb, phi_compare_bb, join_bb);
            }
        }
    }

    // test cases:
        // sanity check
            // with phis
            // no phis
        // complex body
            // if statement
            // loop
            // deeper nesting?
    fn visit_loop(&mut self, loop_stmt: &Loop) {
        let prev_bb = self.current_block.expect("invariant violated: loop must be in block");
        let start_bb = self.make_basic_block_from(prev_bb, prev_bb);
        self.body.connect_via_fallthrough(prev_bb, start_bb);

        let phis = PhiDetectionPass::run(self.body.basic_block_data(start_bb), &loop_stmt.body)
            .into_iter()
            .map(|var| {
                let old_val = self.body.val_in_bb(start_bb, &var).expect("invariant violated: phi detection returned non-existent var");
                let dest_val = self.alloc_val();
                self.body.assign_in_bb(start_bb, var.clone(), dest_val);

                (
                    var,
                    Instruction::StoredBinaryOp(StoredBinaryOpcode::Phi, old_val, Value(0), dest_val),
                )
            })
            .collect::<Vec<_>>();

        let body_bb = self.fill_basic_block_from(start_bb, start_bb);
        self.visit_block(&loop_stmt.body);

        self.body.connect_via_branch(body_bb, start_bb);
        let post_bb = self.make_basic_block_from(start_bb, start_bb);

        self.current_block = Some(start_bb);

        for (var, mut phi) in phis.into_iter() {
            match phi {
                Instruction::StoredBinaryOp(StoredBinaryOpcode::Phi, _, ref mut src2, _) => *src2 = self.body
                    .val_in_bb(body_bb, &var)
                    .expect("invariant violated: val not found for var in phi instruction"),
                _ => unreachable!(),
            }

            self.body.push_instr(start_bb, phi);
        }

        self.visit_relation(&loop_stmt.condition);
        let condition_val = self.last_val.expect("invariant violated: value expected for loop condition");
        let opcode = BranchOpcode::from(loop_stmt.condition.op.negated());

        self.body.set_edge_for_block(start_bb, ControlFlowEdge::Loop(body_bb, post_bb));
        self.body.push_instr(
            start_bb,
            Instruction::Branch(opcode, condition_val, post_bb),
        );
        self.current_block = Some(post_bb);
    }

    // test cases:
        // sanity check each relop
    fn visit_relation(&mut self, relation: &Relation) {
        self.visit_expr(&relation.lhs);
        let lhs = self.last_val.expect("invariant violated: expected expr");

        self.visit_expr(&relation.rhs);
        let rhs = self.last_val.expect("invariant violated: expected expr");

        match self.try_const_compute(StoredBinaryOpcode::Cmp, lhs, rhs) {
            Some(const_result) => self.load_const(const_result),
            None => {
                let block = self.current_block.expect("invariant violated: relation must be in block");
                let cse_index = IndexableInstr::Cmp(lhs, rhs);

                self.last_val = self.cse_cache.as_ref()
                    .and_then(|c| c.get_common_subexpr(&self.body, block, &cse_index))
                    .or_else(|| {
                        let result = self.alloc_val();
                        let instr = Instruction::StoredBinaryOp(StoredBinaryOpcode::Cmp, lhs, rhs, result);

                        if let Some(ref mut cse_cache) = self.cse_cache {
                            cse_cache.insert_instr(block, cse_index, result);
                        }

                        self.body.push_instr(block, instr);

                        Some(result)
                    });
            }
        }
    }

    // test cases:
        // with value
        // no value
    fn visit_return(&mut self, ret: &Return) {
        visit::walk_return(self, ret);
        let block = self.current_block.expect("invariant violated: return must be in block");

        if let Some(ret_val) = self.last_val {
            self.body.push_instr(block, Instruction::Move(ret_val, CCLocation::RetVal));
        }

        self.body.push_instr(block, Instruction::Return);
        self.current_block_returns = true;
    }

    // test cases:
        // sanity check each kind of statement
    fn visit_stmt(&mut self, stmt: &Stmt) {
        if !self.opt.dead_code_elim || !self.current_block_returns {
            visit::walk_stmt(self, stmt);
        }
    }

    // test cases:
        // just one factor
        // just one factor op
        // many factor ops
        // MAKE SURE TO TEST BOTH FACTOR OPS
    fn visit_term(&mut self, term: &Term) {
        self.visit_factor(&term.root);

        for (op, factor) in term.ops.iter() {
            let lhs = self.last_val.expect("invariant violated: expected expr");
            self.visit_factor(factor);
            let rhs = self.last_val.expect("invariant violated: expected expr");
            let opcode = StoredBinaryOpcode::from(*op);

            match self.try_const_compute(opcode, lhs, rhs) {
                Some(result) => self.load_const(result),
                None => {
                    let block = self.current_block.expect("invariant violated: term must be in block");
                    let cse_index = IndexableInstr::from_factor_op(*op, lhs, rhs);

                    self.last_val = self.cse_cache.as_ref()
                        .and_then(|c| c.get_common_subexpr(&self.body, block, &cse_index))
                        .or_else(|| {
                            let result = self.alloc_val();
                            let instr = Instruction::StoredBinaryOp(opcode, lhs, rhs, result);

                            if let Some(ref mut cse_cache) = self.cse_cache {
                                cse_cache.insert_instr(block, cse_index, result);
                            }

                            self.body.push_instr(block, instr);

                            Some(result)
                        });
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashMap;
    use maplit::hashmap;
    use crate::{
        ast::{Return},
        driver::opt::OptLevel,
        ir::isa::{BasicBlock, BasicBlockData},
        utils::Builtin,
    };

    fn make_ir_generator(lvl: OptLevel) -> IrBodyGenerator {
        let opt = OptConfig::from(lvl);
        let mut gen = IrBodyGenerator::new(opt);
        gen.current_block = Some(gen.body.make_new_root());

        gen
    }

    #[test]
    fn assignment_ir_gen() {
        /*
            let x <- 1;
        */

        let mut gen = make_ir_generator(OptLevel::Bare);
        gen.visit_assignment(&Assignment {
            place: "x".to_string(),
            value: Expr {
                root: Term {
                    root: Factor::Number(1),
                    ops: Vec::new(),
                },
                ops: Vec::new(),
            }
        });

        assert_eq!(gen.into_body(), Body {
            blocks: vec![
                BasicBlockData {
                    body: vec![],
                    val_table: hashmap!{"x".to_string() => Value(0)},
                    edge: ControlFlowEdge::Leaf,
                    dominator: Some(BasicBlock(1)),
                },
                BasicBlockData {
                    body: vec![Instruction::Const(1, Value(0))],
                    val_table: HashMap::new(),
                    edge: ControlFlowEdge::Fallthrough(BasicBlock(0)),
                    dominator: None,
                },
            ],
            root: Some(BasicBlock(1)),
        })
    }

    #[test]
    fn empty_block_ir_gen() {
        let mut gen = make_ir_generator(OptLevel::Bare);
        gen.visit_block(&Block {
            body: vec![]
        });

        assert_eq!(gen.into_body(), Body {
            blocks: vec![
                BasicBlockData {
                    body: vec![Instruction::Nop],
                    val_table: HashMap::new(),
                    edge: ControlFlowEdge::Leaf,
                    dominator: None,
                }
            ],
            root: Some(BasicBlock(0)),
        });
    }

    #[test]
    fn single_stmt_block_ir_gen() {
        /*
            {
                return;
            }.
        */

        let mut gen = make_ir_generator(OptLevel::Bare);
        gen.visit_block(&Block {
            body: vec![Stmt::Return(Return { value: None })]
        });

        assert_eq!(gen.into_body(), Body {
            blocks: vec![
                BasicBlockData {
                    body: vec![Instruction::Return],
                    val_table: HashMap::new(),
                    edge: ControlFlowEdge::Leaf,
                    dominator: None,
                }
            ],
            root: Some(BasicBlock(0)),
        });
    }

    #[test]
    fn many_stmt_block_ir_gen() {
        /*
            {
                call OutputNum(5);
                call OutputNewLine();
            }
        */

        let mut gen = make_ir_generator(OptLevel::Bare);
        gen.visit_block(&Block {
            body: vec![
                Stmt::FuncCall(FuncCall {
                    name: Builtin::OutputNum.to_string(),
                    args: vec![Expr {
                        root: Term {
                            root: Factor::Number(5),
                            ops: vec![],
                        },
                        ops: vec![],
                    }]
                }),
                Stmt::FuncCall(FuncCall {
                    name: Builtin::OutputNewLine.to_string(),
                    args: vec![],
                }),
            ]
        });

        assert_eq!(gen.into_body(), Body {
            blocks: vec![
                BasicBlockData {
                    body: vec![Instruction::Write(Value(0)), Instruction::Writeln],
                    val_table: HashMap::new(),
                    edge: ControlFlowEdge::Leaf,
                    dominator: Some(BasicBlock(1)),
                },
                BasicBlockData {
                    body: vec![Instruction::Const(5, Value(0))],
                    val_table: HashMap::new(),
                    edge: ControlFlowEdge::Fallthrough(BasicBlock(0)),
                    dominator: None,
                },
            ],
            root: Some(BasicBlock(1)),
        });
    }

    #[test]
    #[ignore]
    fn dead_code_empty_block_ir_gen() {
        todo!();
    }
}
