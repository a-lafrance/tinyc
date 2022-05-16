pub mod fmt;
mod gen;
pub mod isa;
pub mod visit;

use std::collections::HashMap;
use crate::{ast::Computation, driver::opt::OptConfig, utils::Keyword};
use self::{
    gen::IrGenerator,
    isa::Body,
};

#[derive(Debug, PartialEq)]
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

    pub fn pop_main_body(&mut self) -> Option<Body> {
        self.bodies.remove(&Keyword::Main.to_string())
    }

    pub fn bodies(&self) -> impl Iterator<Item = (&String, &Body)> + '_ {
        self.bodies.iter()
    }

    pub fn register(&mut self, name: String, body: Body) {
        self.bodies.insert(name, body);
    }
}
