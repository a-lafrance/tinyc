mod isa;

use std::{
    collections::HashSet,
    io::{self, BufWriter, Write},
};
use crate::ir::{
    isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, Instruction, StoredBinaryOpcode, Value},
    visit::{self, IrVisitor},
    IrStore,
};

pub fn gen_code<W: Write>(mut ir: IrStore, mut writer: BufWriter<W>) {
    // for each body:
        // traverse the cfg for the body, generating instructions for each bb in order

    // but for now, just main
    let body = ir.main_body().unwrap();
    let mut gen = DlxCodegen::new(&body, &mut writer);
    gen.visit_body(&body);
}

struct DlxCodegen<'b, 'wr, W: Write> {
    body: &'b Body,
    writer: &'wr mut BufWriter<W>,
    visited: HashSet<BasicBlock>,
}

impl<'b, 'wr, W: Write> DlxCodegen<'b, 'wr, W> {
    pub fn new(body: &'b Body, writer: &'wr mut BufWriter<W>) -> DlxCodegen<'b, 'wr, W> {
        DlxCodegen { body, writer, visited: HashSet::new() }
    }
}

impl<W: Write> IrVisitor for DlxCodegen<'_, '_, W> {
    fn visit_body(&mut self, body: &Body) {
        if let Some((root, root_bb)) = body.root_block_data() {
            self.visit_basic_block(root, root_bb);
        }
    }

    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        // emit label for basic block
        // TODO: disambiguate blocks based on body name
        if !self.visited.contains(&bb) {
            self.visited.insert(bb);
            writeln!(self.writer, "{}:", bb);
            visit::walk_basic_block(self, bb_data);

            if let Some(ft_dest) = bb_data.fallthrough_dest() {
                let ft_dest_data = self.body.basic_block_data(ft_dest);
                self.visit_basic_block(ft_dest, ft_dest_data);
            }

            if let Some(br_dest) = bb_data.branch_dest() {
                let br_dest_data = self.body.basic_block_data(br_dest);
                self.visit_basic_block(br_dest, br_dest_data);
            }
        }
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        write!(self.writer, "  ");
        visit::walk_instr(self, instr);
    }

    fn visit_branch_instr(&mut self, _opcode: BranchOpcode, _dest: BasicBlock) {

    }

    fn visit_call_instr(&mut self, _func: &str) {

    }

    fn visit_cmp_instr(&mut self, _lhs: Value, _rhs: Value) {

    }

    fn visit_const_instr(&mut self, _const_val: u32, _dest: Value) {

    }

    fn visit_end_instr(&mut self) {

    }

    fn visit_mu_instr(&mut self, _val: Value, _loc: CCLocation) {

    }

    fn visit_nop_instr(&mut self) {

    }

    fn visit_read_instr(&mut self, _dest: Value) {

    }

    fn visit_return_instr(&mut self) {

    }

    fn visit_stored_binop_instr(&mut self, _opcode: StoredBinaryOpcode, _src1: Value, _src2: Value, _dest: Value) {

    }

    fn visit_write_instr(&mut self, _src: Value) {

    }

    fn visit_writeln_instr(&mut self) {

    }
}
