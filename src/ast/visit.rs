use super::{Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop, Relation, Return, Stmt, Term, VarDecl};

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

    fn visit_var_decl(&mut self, decl: &VarDecl) {
        walk_var_decl(self, decl);
    }
}

pub fn walk_assignment(visitor: &mut impl AstVisitor, assign: &Assignment) {

}

pub fn walk_block(visitor: &mut impl AstVisitor, block: &Block) {

}

pub fn walk_computation(visitor: &mut impl AstVisitor, comp: &Computation) {

}

pub fn walk_expr(visitor: &mut impl AstVisitor, expr: &Expr) {

}

pub fn walk_factor(visitor: &mut impl AstVisitor, factor: &Factor) {

}

pub fn walk_func_call(visitor: &mut impl AstVisitor, call: &FuncCall) {

}

pub fn walk_func_decl(visitor: &mut impl AstVisitor, decl: &FuncDecl) {

}

pub fn walk_if_stmt(visitor: &mut impl AstVisitor, if_stmt: &IfStmt) {

}

pub fn walk_loop(visitor: &mut impl AstVisitor, loop_stmt: &Loop) {

}

pub fn walk_relation(visitor: &mut impl AstVisitor, relation: &Relation) {

}

pub fn walk_return(visitor: &mut impl AstVisitor, ret: &Return) {

}

pub fn walk_stmt(visitor: &mut impl AstVisitor, stmt: &Stmt) {
    
}

pub fn walk_term(visitor: &mut impl AstVisitor, term: &Term) {

}

pub fn walk_var_decl(visitor: &mut impl AstVisitor, decl: &VarDecl) {

}
