pub mod color;
pub mod simple;

use std::collections::HashMap;
use dlx::isa::Register;
use crate::ir::isa::{Body, CCLocation, Value};

pub trait Allocator<R: RegisterSet> {
    fn build_table(table: &mut LocationTable<R>, body: &Body);
}

// "Location table" that links IR values to their storage location (register or stack)
#[derive(Debug)]
pub struct LocationTable<R: RegisterSet> {
    mapping: HashMap<Value, Location<R>>,
    cc_mapping: HashMap<CCLocation, Location<R>>,
    total_local_offset: isize,
    total_arg_offset: isize,
}

impl<R: RegisterSet> LocationTable<R> {
    pub fn alloc_from<Alloc: Allocator<R>>(body: &Body) -> Self {
        let mut table = LocationTable::default();
        Alloc::build_table(&mut table, body);

        table
    }

    pub fn get(&self, val: Value) -> Option<Location<R>> {
        self.mapping.get(&val).copied()
    }

    pub fn get_from_cc(&self, cc_loc: CCLocation) -> Option<Location<R>> {
        self.cc_mapping.get(&cc_loc).copied()
    }

    pub fn total_local_offset(&self) -> isize {
        self.total_local_offset
    }

    pub fn total_arg_offset(&self) -> isize {
        self.total_arg_offset
    }

    pub fn stack_in_use(&self) -> bool {
        self.total_local_offset() != 0 || self.total_arg_offset() != 0
    }

    // Importantly, don't expose a mutable API to third-parties
    pub(self) fn insert(&mut self, val: Value, loc: Location<R>) {
        self.mapping.insert(val, loc);

        if let Location::Stack(offset) = loc {
            if offset > self.total_local_offset {
                self.total_local_offset = offset;
            } else if offset < self.total_arg_offset {
                self.total_arg_offset = offset;
            }
        }
    }

    pub(self) fn insert_from_cc(&mut self, val: Value, loc: Location<R>, cc_loc: CCLocation) {
        self.insert(val, loc);
        self.cc_mapping.insert(cc_loc, loc);
    }
}

impl<R: RegisterSet> Default for LocationTable<R> {
    fn default() -> Self {
        LocationTable {
            mapping: HashMap::default(),
            cc_mapping: HashMap::default(),
            total_local_offset: 0,
            total_arg_offset: 0,
        }
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

    // Returns the register for the given location based on calling conventions
    fn for_cc_location(loc: CCLocation) -> Option<Self>;
}

// The argument for implementing it here is that it keeps compiler-specific register set info internal
// and establishes the DLX calling conventions as compiler-enforced, rather than inherent to the architecture
// There's an argument for both sides though
impl RegisterSet for Register {
    fn from_index(i: u8) -> Option<Self> {
        const FIRST_GP_REG: u8 = 1;
        const LAST_GP_REG: u8 = 19;

        let true_index = i + 1; // skip the zero register

        if (FIRST_GP_REG..=LAST_GP_REG).contains(&true_index) {
            Some(Register(true_index))
        } else {
            None
        }
    }

    fn for_cc_location(loc: CCLocation) -> Option<Self> {
        const FIRST_ARG_REG: u8 = 20;
        const RET_VAL_REG: Register = Register(27);
        const N_REG_ARGS: usize = 5;

        match loc {
            CCLocation::RetVal => Some(RET_VAL_REG),
            CCLocation::Arg(n) if n < N_REG_ARGS => Some(Register(FIRST_ARG_REG + n as u8)),
            _ => None,
        }
    }
}
