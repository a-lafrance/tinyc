use crate::ir::{
    isa::{Body, Instruction},
    visit::{self, IrVisitor},
};
use super::{Allocator, Location, LocationTable, RegisterSet};

pub struct SimpleAllocator;

impl<R: RegisterSet> Allocator<R> for SimpleAllocator {
    fn build_table(table: &mut LocationTable<R>, body: &Body) {
        let mut builder = LocationTableBuilder::new(table);
        builder.visit_body(body);
    }
}

struct LocationTableBuilder<'t, R: RegisterSet> {
    table: &'t mut LocationTable<R>,
    next_reg_index: u8,
    next_stack_local_offset: isize,
    next_stack_arg_offset: isize,
}

impl<'t, R: RegisterSet> LocationTableBuilder<'t, R> {
    pub fn new(table: &'t mut LocationTable<R>) -> Self {
        LocationTableBuilder {
            table,
            next_reg_index: 0,
            next_stack_local_offset: 1,
            next_stack_arg_offset: -1,
        }
    }
}

impl<R: RegisterSet> IrVisitor for LocationTableBuilder<'_, R> {
    fn visit_body(&mut self, body: &Body) {
        visit::walk_body(self, body);
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Bind(val, cc_loc) | Instruction::Move(val, cc_loc) => {
                let loc = match R::for_cc_location(*cc_loc) {
                    Some(reg) => Location::Reg(reg),
                    None => {
                        let loc = Location::Stack(self.next_stack_arg_offset);
                        self.next_stack_arg_offset -= 1;
                        loc
                    }
                };

                self.table.insert_from_cc(*val, loc, *cc_loc);
            },

            instr => if let Some(val) = instr.result_val() {
                let loc = match R::from_index(self.next_reg_index) {
                    Some(reg) => {
                        self.next_reg_index = self.next_reg_index.saturating_add(1);
                        Location::Reg(reg)
                    },

                    None => {
                        let loc = Location::Stack(self.next_stack_local_offset);
                        self.next_stack_local_offset += 1;
                        loc
                    },
                };

                self.table.insert(val, loc);
            }
        }
    }
}
