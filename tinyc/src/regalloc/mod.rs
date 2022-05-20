// mod graph;

use std::collections::HashMap;
use dlx::isa::Register;
use crate::ir::{
    isa::{Body, Instruction, Value},
    visit::{self, IrVisitor},
};

// "Location table" that links IR values to their storage location (register or stack)
#[derive(Debug)]
pub struct LocationTable<R: RegisterSet> {
    mapping: HashMap<Value, Location<R>>,
}

impl<R: RegisterSet> LocationTable<R> {
    pub fn alloc_from(body: &Body) -> Self {
        let mut table = LocationTable::default();
        let mut builder = LocationTableBuilder::new(&mut table);
        builder.visit_body(body);

        table
    }

    pub fn get(&self, val: Value) -> Option<Location<R>> {
        self.mapping.get(&val).copied()
    }

    // Importantly, don't expose a mutable API to third-parties
    pub(self) fn insert(&mut self, val: Value, loc: Location<R>) {
        self.mapping.insert(val, loc);
    }
}

impl<R: RegisterSet> Default for LocationTable<R> {
    fn default() -> Self {
        LocationTable { mapping: HashMap::default() }
    }
}

// An storage location, either in a register or on the stack
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Location<R: RegisterSet> {
    Reg(R), // register used for storage (architecture-specific)
    Stack(isize), // offset (in increments of 4 bytes) from the stack pointer
}

// Behavior required by a type representing an architecture's (general-purpose) register set
// Register sets _must_ be lightweight enough to copy (this should be a very easy restriction to follow)
pub trait RegisterSet: Sized + Copy {
    // Returns the register for the given index, if it exists in the register set
    fn from_index(i: u8) -> Option<Self>;
}

// The argument for implementing it here is that it keeps compiler-specific register set info internal
// and establishes the DLX calling conventions as compiler-enforced, rather than inherent to the architecture
// There's an argument for both sides though
impl RegisterSet for Register {
    fn from_index(i: u8) -> Option<Self> {
        const FIRST_GP_REG: u8 = 1;
        const LAST_GP_REG: u8 = 19;

        let true_index = i + 1; // skip the zero register

        if true_index >= FIRST_GP_REG && true_index <= LAST_GP_REG {
            Some(Register(true_index))
        } else {
            None
        }
    }
}


struct LocationTableBuilder<'t, R: RegisterSet> {
    table: &'t mut LocationTable<R>,
    next_reg_index: u8,
    next_stack_offset: isize,
}

impl<'t, R: RegisterSet> LocationTableBuilder<'t, R> {
    pub fn new(table: &'t mut LocationTable<R>) -> Self {
        LocationTableBuilder {
            table,
            next_reg_index: 0,
            next_stack_offset: 1,
        }
    }
}

impl<R: RegisterSet> IrVisitor for LocationTableBuilder<'_, R> {
    fn visit_body(&mut self, body: &Body) {
        visit::walk_body(self, body);
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        if let Some(val) = instr.result_val() {
            let loc = match R::from_index(self.next_reg_index) {
                Some(reg) => {
                    self.next_reg_index = self.next_reg_index.saturating_add(1);
                    Location::Reg(reg)
                },

                None => {
                    let loc = Location::Stack(self.next_stack_offset);
                    self.next_stack_offset += 1;
                    loc
                },
            };

            self.table.insert(val, loc);
        }
    }
}
