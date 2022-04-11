use std::collections::HashMap;
use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Relation, Term, VarDecl,
        visit::{self, AstVisitor},
    },
    utils::Builtin,
};
use super::{BasicBlock, BranchOpcode, InstructionData, IrStore, StoredBinaryOpcode, Value};

pub struct IrGenerator {
    store: IrStore,
    val_table: HashMap<String, Value>, // generalize to one per bb
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
            val_table: HashMap::new(),
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
        self.last_val = self.val_table.get(var).copied();
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
}

impl AstVisitor for IrGenerator {
    fn visit_assignment(&mut self, assign: &Assignment) {
        self.visit_expr(&assign.value);
        self.val_table.insert(
            assign.place.clone(),
            self.last_val.expect("invariant violated: assignment to non-expression"),
        );
    }

    fn visit_block(&mut self, block: &Block) {
        let bb = self.current_block.expect("invariant violated: basic block not created for block");

        if block.is_empty() {
            self.store.push_instr(bb, InstructionData::Nop);
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
            InstructionData::End
        );
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.visit_term(&expr.root);

        for (op, term) in expr.ops.iter() {
            let lhs = self.last_val.expect("invariant violated: expected expr");
            self.visit_term(term);
            let rhs = self.last_val.expect("invariant violated: expected expr");
            let result = self.alloc_val();
            let instr = InstructionData::StoredBinaryOp {
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

                InstructionData::Read(val)
            },
            Some(Builtin::OutputNum) => {
                self.visit_expr(&call.args[0]); // FIXME: a bit unsafe
                InstructionData::Write(self.last_val.expect("invariant violated: expected expr"))
            },
            Some(Builtin::OutputNewLine) => InstructionData::Writeln,
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
        let then_bb = self.fill_basic_block();
        self.visit_block(&if_stmt.then_block);

        // connect start basic block to then basic block via fallthrough
        self.store.connect_via_fallthrough(condition_bb, then_bb);

        // pre-allocate join basic block
        let join_bb = self.store.make_new_basic_block();

        // connect inner blocks together depending on presence of else
        let dest_bb = match if_stmt.else_block {
            Some(ref else_block) => {
                // if else block exists, fill new basic block for it
                let else_bb = self.fill_basic_block();
                self.visit_block(else_block);

                // connect then block to join block via branch
                // connect else block to join block via fallthrough
                self.store.connect_via_branch(then_bb, join_bb, BranchOpcode::Br);
                self.store.connect_via_fallthrough(else_bb, join_bb);
                else_bb
            },

            None => {
                // connect then block to join block via fallthrough
                self.store.connect_via_fallthrough(then_bb, join_bb);
                join_bb
            },
        };

        // connect start block to destination block (either join or else) via conditional branch
        let branch_opcode = BranchOpcode::from(if_stmt.condition.op.negated());
        self.store.connect_via_branch(condition_bb, dest_bb, branch_opcode);

        // fast-forward to join block
        self.current_block = Some(join_bb);

        // generate phi instructions in end block
            // compare value tables from then/else and resolve differences
                // if there's no else, else block is effectively the start block, ie did any values change during the then block
            // IMPORTANT: this requires using the _end_ basic block for then/else, not the start
    }

    // fn visit_loop(&mut self, loop_stmt: &Loop) {
    //     walk_loop(self, loop_stmt);
    // }

    fn visit_relation(&mut self, relation: &Relation) {
        self.visit_expr(&relation.lhs);
        let lhs = self.last_val.expect("invariant violated: expected expr");

        self.visit_expr(&relation.rhs);
        let rhs = self.last_val.expect("invariant violated: expected expr");

        self.store.push_instr(
            self.current_block.expect("invariant violated: func call must be in block"),
            InstructionData::Cmp(lhs, rhs)
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
            let instr = InstructionData::StoredBinaryOp {
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

    fn visit_var_decl(&mut self, _: &VarDecl) { }
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
            store.push_instr(prelude_block, InstructionData::Const(n, val));
        }

        if let Some(root) = store.root_block() {
            store.connect_via_fallthrough(prelude_block, root);
        }

        store.set_root_block(prelude_block);
    }
}
