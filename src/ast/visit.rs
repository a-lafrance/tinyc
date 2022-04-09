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

    fn visit_var_decl(&mut self, decl: &VarDecl);
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
    for var_decl in comp.vars.iter() {
        visitor.visit_var_decl(var_decl);
    }

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
    for var_decl in decl.vars.iter() {
        visitor.visit_var_decl(var_decl);
    }

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
