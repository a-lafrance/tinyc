use std::collections::HashMap;
use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Relation, Term, VarDecl,
        visit::{self, AstVisitor},
    },
    utils::Builtin,
};
use super::{BasicBlock, InstructionData, IrStore, StoredBinaryOpcode, Value};

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

    pub fn store(&self) -> &IrStore {
        &self.store
    }

    pub fn store_mut(&mut self) -> &mut IrStore {
        &mut self.store
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
        let new_block = self.store.make_new_basic_block();
        self.current_block = Some(new_block);

        if block.is_empty() {
            self.store.push_instr(new_block, InstructionData::Nop);
        } else {
            visit::walk_block(self, block);
        }
    }

    fn visit_computation(&mut self, comp: &Computation) {
        visit::walk_computation(self, comp);

        let main_block = self.current_block.expect("invariant violated: missing main block");

        self.store.root_block_mut().insert(main_block);
        self.store.push_instr(
            main_block,
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
            Some(Builtin::InputNum) => InstructionData::Read(self.alloc_val()),
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
        // check condition in current block
        // generate branch instr in current block
        // save current block for use later
        // visit then block

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

        let root = *store.root_block();
        let prelude_block_data = store.basic_block_data_mut(prelude_block);
        *prelude_block_data.fallthrough_dest_mut() = root;

        *store.root_block_mut() = Some(prelude_block);
    }
}