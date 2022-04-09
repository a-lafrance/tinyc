/*
    main
    var a, b, c, n;

    {
        let a <- call InputNum();
        let b <- call InputNum();
        let n <- 2;
        let c <- a + b;
        call OutputNum(c * n);
        call OutputNL();
    }.
*/

// BB0:
// a = $0
// b = $1
// c = $2
    // $0 = read
    // $1 = read
    // $2 = add $0, $1
    // $4 = mul $2, $3
    // write $4
    // writeln
// const map:
// n = $3
    // $3 = const 2

// final IR:
// BBp: "Basic Block Prelude"
    // $3 = const 2
// BB0:
    // $0 = read
    // $1 = read
    // $2 = add $0, $1
    // $4 = mul $2, $3
    // write $4
    // writeln
    // end

// func call:
    // if builtin, generate io primitive instr
    // else, unimplemented error

use std::collections::HashMap;
use crate::ast::{
    Assignment, Computation, Factor, FactorOp, Term, VarDecl,
    visit::{self, AstVisitor},
};
use super::{BasicBlock, InstructionData, IrStore, StoredBinaryOpcode, Value};

pub struct IrGenerator {
    store: IrStore,
    val_table: HashMap<String, Value>, // generalize to one per bb
    const_alloc: ConstAllocator,
    last_val: Option<Value>,
    next_val: Value,
}

impl IrGenerator {
    pub fn gen(ast: &Computation) -> IrStore {
        let mut gen = IrGenerator::new();
        gen.visit_computation(ast);
        // convert the const allocator into a basic block

        gen.store
    }

    fn new() -> IrGenerator {
        IrGenerator {
            store: IrStore::new(),
            val_table: HashMap::new(),
            const_alloc: ConstAllocator::new(),
            last_val: None,
            next_val: Value(0),
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
}

impl AstVisitor for IrGenerator {
    fn visit_assignment(&mut self, assign: &Assignment) {
        self.visit_expr(&assign.value);
        self.val_table.insert(
            assign.place.clone(),
            self.last_val.expect("invariant violated: assignment to non-expression"),
        );
    }

    // fn visit_block(&mut self, block: &Block) {
    //     walk_block(self, block);
    // }
    //
    // fn visit_computation(&mut self, comp: &Computation) {
    //     walk_computation(self, comp);
    // }
    //
    // fn visit_expr(&mut self, expr: &Expr) {
    //     walk_expr(self, expr);
    // }

    fn visit_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::VarRef(var) => self.load_var(var),
            Factor::Number(n) => self.load_const(*n),
            _ => visit::walk_factor(self, factor),
        }

        match self.last_val {
            Some(ref val) => eprintln!("loaded {}", val),
            None => eprintln!("no val loaded"),
        }
    }

    // fn visit_func_call(&mut self, call: &FuncCall) {
    //     walk_func_call(self, call);
    // }
    //
    // fn visit_func_decl(&mut self, decl: &FuncDecl) {
    //     walk_func_decl(self, decl);
    // }
    //
    // fn visit_if_stmt(&mut self, if_stmt: &IfStmt) {
    //     walk_if_stmt(self, if_stmt);
    // }
    //
    // fn visit_loop(&mut self, loop_stmt: &Loop) {
    //     walk_loop(self, loop_stmt);
    // }
    //
    // fn visit_relation(&mut self, relation: &Relation) {
    //     walk_relation(self, relation);
    // }
    //
    // fn visit_return(&mut self, ret: &Return) {
    //     walk_return(self, ret);
    // }
    //
    // fn visit_stmt(&mut self, stmt: &Stmt) {
    //     walk_stmt(self, stmt);
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

            eprintln!("gen instr: {}", instr);

            // somehow add the instr to both the store and the bb
            self.last_val = Some(result)
        }

        // ^ expr will literally be exactly that but slightly different
    }

    fn visit_var_decl(&mut self, decl: &VarDecl) { }
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

    pub fn to_basic_block(self, _ctx: &mut IrGenerator) -> BasicBlock {
        todo!()
    }
}
