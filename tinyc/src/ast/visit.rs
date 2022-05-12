use super::{Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop, Relation, Return, Stmt, Term};

// Credit where credit is due, the design of this visitor system is pretty heavily inspired by Rustc's own AST visitor.
// If you're into that sort of thing, take a look here: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_ast/visit/trait.Visitor.html

pub trait AstVisitor: Sized {
    fn visit_assignment(&mut self, assign: &Assignment) {
        walk_assignment(self, assign);
    }

    fn visit_block(&mut self, block: &Block) {
        walk_block(self, block);
    }

    fn visit_computation(&mut self, comp: &Computation) {
        walk_computation(self, comp);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_factor(&mut self, factor: &Factor) {
        walk_factor(self, factor);
    }

    fn visit_func_call(&mut self, call: &FuncCall) {
        walk_func_call(self, call);
    }

    fn visit_func_decl(&mut self, decl: &FuncDecl) {
        walk_func_decl(self, decl);
    }

    fn visit_if_stmt(&mut self, if_stmt: &IfStmt) {
        walk_if_stmt(self, if_stmt);
    }

    fn visit_loop(&mut self, loop_stmt: &Loop) {
        walk_loop(self, loop_stmt);
    }

    fn visit_relation(&mut self, relation: &Relation) {
        walk_relation(self, relation);
    }

    fn visit_return(&mut self, ret: &Return) {
        walk_return(self, ret);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_term(&mut self, term: &Term) {
        walk_term(self, term);
    }
}

pub fn walk_assignment(visitor: &mut impl AstVisitor, assign: &Assignment) {
    visitor.visit_expr(&assign.value);
}

pub fn walk_block(visitor: &mut impl AstVisitor, block: &Block) {
    for stmt in block.body.iter() {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_computation(visitor: &mut impl AstVisitor, comp: &Computation) {
    for func_decl in comp.funcs.iter() {
        visitor.visit_func_decl(func_decl);
    }

    visitor.visit_block(&comp.body);
}

pub fn walk_expr(visitor: &mut impl AstVisitor, expr: &Expr) {
    visitor.visit_term(&expr.root);

    for (_, term) in expr.ops.iter() {
        visitor.visit_term(term);
    }
}

pub fn walk_factor(visitor: &mut impl AstVisitor, factor: &Factor) {
    match factor {
        Factor::SubExpr(subexpr) => visitor.visit_expr(subexpr),
        Factor::Call(call) => visitor.visit_func_call(call),
        _ => {},
    }
}

pub fn walk_func_call(visitor: &mut impl AstVisitor, call: &FuncCall) {
    for arg in call.args.iter() {
        visitor.visit_expr(arg);
    }
}

pub fn walk_func_decl(visitor: &mut impl AstVisitor, decl: &FuncDecl) {
    visitor.visit_block(&decl.body);
}

pub fn walk_if_stmt(visitor: &mut impl AstVisitor, if_stmt: &IfStmt) {
    visitor.visit_relation(&if_stmt.condition);
    visitor.visit_block(&if_stmt.then_block);

    if let Some(ref block) = if_stmt.else_block {
        visitor.visit_block(block);
    }
}

pub fn walk_loop(visitor: &mut impl AstVisitor, loop_stmt: &Loop) {
    visitor.visit_relation(&loop_stmt.condition);
    visitor.visit_block(&loop_stmt.body);
}

pub fn walk_relation(visitor: &mut impl AstVisitor, relation: &Relation) {
    visitor.visit_expr(&relation.lhs);
    visitor.visit_expr(&relation.rhs);
}

pub fn walk_return(visitor: &mut impl AstVisitor, ret: &Return) {
    if let Some(ref ret_val) = ret.value {
        visitor.visit_expr(ret_val);
    }
}

pub fn walk_stmt(visitor: &mut impl AstVisitor, stmt: &Stmt) {
    match stmt {
        Stmt::Assignment(assign) => visitor.visit_assignment(assign),
        Stmt::FuncCall(call) => visitor.visit_func_call(call),
        Stmt::If(if_stmt) => visitor.visit_if_stmt(if_stmt),
        Stmt::Loop(loop_stmt) => visitor.visit_loop(loop_stmt),
        Stmt::Return(ret) => visitor.visit_return(ret),
    }
}

pub fn walk_term(visitor: &mut impl AstVisitor, term: &Term) {
    visitor.visit_factor(&term.root);

    for (_, factor) in term.ops.iter() {
        visitor.visit_factor(factor);
    }
}


#[cfg(test)]
mod tests {
    // use an ast visitor that just adds to a giant string
    // make sure the string matches the expected string at the end

    use super::*;
    use std::string::ToString;
    use crate::{ast::{FactorOp, TermOp}, utils::RelOp};

    struct VisitChecker(String);

    impl VisitChecker {
        pub fn new() -> Self {
            VisitChecker(String::new())
        }
    }

    impl AstVisitor for VisitChecker {
        fn visit_assignment(&mut self, assign: &Assignment) {
            self.0.push('a');
            walk_assignment(self, assign);
        }

        fn visit_block(&mut self, block: &Block) {
            self.0.push('b');
            walk_block(self, block);
        }

        fn visit_computation(&mut self, comp: &Computation) {
            self.0.push_str("main");
            walk_computation(self, comp);
        }

        fn visit_expr(&mut self, expr: &Expr) {
            self.0.push('x');
            walk_expr(self, expr);
        }

        fn visit_factor(&mut self, factor: &Factor) {
            self.0.push('f');
            walk_factor(self, factor);
        }

        fn visit_func_call(&mut self, call: &FuncCall) {
            self.0.push_str("fc");
            walk_func_call(self, call);
        }

        fn visit_func_decl(&mut self, decl: &FuncDecl) {
            self.0.push_str("fd");
            walk_func_decl(self, decl);
        }

        fn visit_if_stmt(&mut self, if_stmt: &IfStmt) {
            self.0.push_str("if");
            walk_if_stmt(self, if_stmt);
        }

        fn visit_loop(&mut self, loop_stmt: &Loop) {
            self.0.push('l');
            walk_loop(self, loop_stmt);
        }

        fn visit_relation(&mut self, relation: &Relation) {
            self.0.push('r');
            walk_relation(self, relation);
        }

        fn visit_return(&mut self, ret: &Return) {
            self.0.push_str("ret");
            walk_return(self, ret);
        }

        fn visit_stmt(&mut self, stmt: &Stmt) {
            self.0.push('s');
            walk_stmt(self, stmt);
        }

        fn visit_term(&mut self, term: &Term) {
            self.0.push('t');
            walk_term(self, term);
        }
    }

    #[test]
    fn walk_assignment_sanity_check() {
        let mut v = VisitChecker::new();
        let assign = Assignment {
            place: "x".to_string(),
            value: Expr {
                root: Term {
                    root: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            }
        };

        v.visit_assignment(&assign);
        assert_eq!(v.0, "axtf");
    }

    #[test]
    fn walk_block_sanity_check() {
        let mut v = VisitChecker::new();
        let block = Block {
            body: vec![
                Stmt::Return(Return { value: None }),
                Stmt::Return(Return { value: None }),
                Stmt::Return(Return { value: None }),
            ]
        };

        v.visit_block(&block);
        assert_eq!(v.0, "bsretsretsret");
    }

    #[test]
    fn walk_computation_sanity_check() {
        /*
            main
            var a;
            var b;

            void function empty();
            {

            }

            void function empty2();
            {

            }

            {

            }.
        */

        let mut v = VisitChecker::new();
        let comp = Computation {
            funcs: vec![
                FuncDecl {
                    returns_void: true,
                    name: "empty".to_string(),
                    params: vec![],
                    body: Block::empty(),
                },
                FuncDecl {
                    returns_void: true,
                    name: "empty2".to_string(),
                    params: vec![],
                    body: Block::empty(),
                },
            ],
            body: Block::empty()
        };

        v.visit_computation(&comp);
        assert_eq!(v.0, "mainfdbfdbb");
    }

    #[test]
    fn walk_expr_single_term_sanity_check() {
        let mut v = VisitChecker::new();
        let expr = Expr {
            root: Term {
                root: Factor::Number(2),
                ops: vec![],
            },
            ops: vec![]
        };

        v.visit_expr(&expr);
        assert_eq!(v.0, "xtf");
    }

    #[test]
    fn walk_expr_many_terms_sanity_check() {
        let mut v = VisitChecker::new();
        let expr = Expr {
            root: Term {
                root: Factor::Number(2),
                ops: vec![],
            },
            ops: vec![
                (TermOp::Add, Term {
                    root: Factor::Number(1),
                    ops: vec![],
                })
            ]
        };

        v.visit_expr(&expr);
        assert_eq!(v.0, "xtftf");
    }

    #[test]
    fn walk_factor_subexpr_sanity_check() {
        let mut v = VisitChecker::new();
        let factor = Factor::SubExpr(Box::new(Expr {
            root: Term {
                root: Factor::Number(1),
                ops: vec![],
            },
            ops: vec![],
        }));

        v.visit_factor(&factor);
        assert_eq!(v.0, "fxtf");
    }

    #[test]
    fn walk_factor_func_call_sanity_check() {
        let mut v = VisitChecker::new();
        let factor = Factor::Call(FuncCall {
            name: "InputNum".to_string(),
            args: vec![]
        });

        v.visit_factor(&factor);
        assert_eq!(v.0, "ffc");
    }

    #[test]
    fn walk_func_call_sanity_check() {
        let mut v = VisitChecker::new();
        let call = FuncCall {
            name: "add".to_string(),
            args: vec![
                Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                    },
                Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            ],
        };

        v.visit_func_call(&call);
        assert_eq!(v.0, "fcxtfxtf");
    }

    #[test]
    fn walk_func_decl_sanity_check() {
        let mut v = VisitChecker::new();
        let decl = FuncDecl {
            returns_void: true,
            name: "empty".to_string(),
            params: vec![],
            body: Block::empty(),
        };

        v.visit_func_decl(&decl);
        assert_eq!(v.0, "fdb");
    }

    #[test]
    fn walk_if_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let if_stmt = IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Eq,
            },
            then_block: Block::empty(),
            else_block: Some(Block::empty()),
        };

        v.visit_if_stmt(&if_stmt);
        assert_eq!(v.0, "ifrxtfxtfbb");
    }

    #[test]
    fn walk_if_stmt_no_else_sanity_check() {
        let mut v = VisitChecker::new();
        let if_stmt = IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Eq,
            },
            then_block: Block::empty(),
            else_block: None,
        };

        v.visit_if_stmt(&if_stmt);
        assert_eq!(v.0, "ifrxtfxtfb");
    }

    #[test]
    fn walk_loop_sanity_check() {
        let mut v = VisitChecker::new();
        let loop_stmt = Loop {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Eq,
            },
            body: Block::empty(),
        };

        v.visit_loop(&loop_stmt);
        assert_eq!(v.0, "lrxtfxtfb");
    }

    #[test]
    fn walk_relation_sanity_check() {
        let mut v = VisitChecker::new();
        let relation = Relation {
            lhs: Expr {
                root: Term {
                    root: Factor::VarRef("x".to_string()),
                    ops: vec![],
                },
                ops: vec![],
            },
            rhs: Expr {
                root: Term {
                    root: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            },
            op: RelOp::Eq,
        };

        v.visit_relation(&relation);
        assert_eq!(v.0, "rxtfxtf");
    }

    #[test]
    fn walk_return_sanity_check() {
        let mut v = VisitChecker::new();
        let ret = Return {
            value: Some(Expr {
                root: Term {
                    root: Factor::VarRef("x".to_string()),
                    ops: vec![],
                },
                ops: vec![],
            }),
        };

        v.visit_return(&ret);
        assert_eq!(v.0, "retxtf");
    }

    #[test]
    fn walk_return_no_val_sanity_check() {
        let mut v = VisitChecker::new();
        let ret = Return { value: None };

        v.visit_return(&ret);
        assert_eq!(v.0, "ret");
    }

    #[test]
    fn walk_assignment_as_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let stmt = Stmt::Assignment(Assignment {
            place: "x".to_string(),
            value: Expr {
                root: Term {
                    root: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            }
        });

        v.visit_stmt(&stmt);
        assert_eq!(v.0, "saxtf");
    }

    #[test]
    fn walk_func_call_as_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let stmt = Stmt::FuncCall(FuncCall {
            name: "InputNum".to_string(),
            args: vec![],
        });

        v.visit_stmt(&stmt);
        assert_eq!(v.0, "sfc");
    }

    #[test]
    fn walk_if_as_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let stmt = Stmt::If(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Eq,
            },
            then_block: Block::empty(),
            else_block: None,
        });

        v.visit_stmt(&stmt);
        assert_eq!(v.0, "sifrxtfxtfb");
    }

    #[test]
    fn walk_loop_as_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let stmt = Stmt::Loop(Loop {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Eq,
            },
            body: Block::empty(),
        });

        v.visit_stmt(&stmt);
        assert_eq!(v.0, "slrxtfxtfb");
    }

    #[test]
    fn walk_return_as_stmt_sanity_check() {
        let mut v = VisitChecker::new();
        let stmt = Stmt::Return(Return { value: None });

        v.visit_stmt(&stmt);
        assert_eq!(v.0, "sret");
    }

    #[test]
    fn walk_term_single_factor_sanity_check() {
        let mut v = VisitChecker::new();
        let term = Term {
            root: Factor::Number(1),
            ops: vec![],
        };

        v.visit_term(&term);
        assert_eq!(v.0, "tf");
    }

    #[test]
    fn walk_term_many_factors_sanity_check() {
        let mut v = VisitChecker::new();
        let term = Term {
            root: Factor::Number(1),
            ops: vec![(FactorOp::Mul, Factor::Number(2))],
        };

        v.visit_term(&term);
        assert_eq!(v.0, "tff");
    }
}
