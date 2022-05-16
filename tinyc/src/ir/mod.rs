pub mod fmt;
mod gen;
pub mod isa;
pub mod opt;
pub mod visit;

use std::collections::HashMap;
use crate::{ast::Computation, utils::Keyword};
use self::{
    gen::IrGenerator,
    isa::Body,
    opt::OptConfig,
};

// NOTE: constant folding/propagation notes
    // buffer a new prelude block for new constants
    // go through all instructions in body, keeping track of constants as you go:
        // keep track of constants in a table of {value : const value}, eg {$0 : 1}
        // if const, add to const table
        // otherwise, if it's a const foldable instruction:
            // if both operands are const, perform calculation (fold consts)
            // if const already has value, substitute that value
            // otherwise, buffer new constant for folded result
            // propagate new constant's value in all future instructions
        // you may have to continually repeat this pass until you get no new edits, to continue the propagation

#[derive(Debug)]
pub struct IrStore {
    bodies: HashMap<String, Body>,
}

impl IrStore {
    pub fn new() -> IrStore {
        IrStore { bodies: HashMap::new() }
    }

    pub fn from_ast(ast: Computation, opt: OptConfig) -> IrStore {
        IrGenerator::gen(&ast, opt)
    }

    // NOTE: this is TEMPORARY
    pub fn main_body(&mut self) -> Option<Body> {
        self.bodies.remove(&Keyword::Main.to_string())
    }

    pub fn bodies(&self) -> &HashMap<String, Body> {
        &self.bodies
    }

    pub fn register(&mut self, name: String, body: Body) {
        self.bodies.insert(name, body);
    }
}
