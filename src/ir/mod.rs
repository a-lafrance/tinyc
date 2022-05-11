pub mod fmt;
mod gen;
pub mod isa;
pub mod visit;

use std::collections::HashMap;
use crate::ast::Computation;
use self::{
    gen::IrGenerator,
    isa::Body,
};

#[derive(Debug)]
pub struct IrStore {
    bodies: HashMap<String, Body>,
}

impl IrStore {
    pub fn new() -> IrStore {
        IrStore { bodies: HashMap::new() }
    }

    pub fn bodies(&self) -> &HashMap<String, Body> {
        &self.bodies
    }

    pub fn register(&mut self, name: String, body: Body) {
        self.bodies.insert(name, body);
    }
}

impl From<Computation> for IrStore {
    fn from(comp: Computation) -> IrStore {
        IrGenerator::gen(&comp)
    }
}
