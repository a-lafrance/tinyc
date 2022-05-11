mod isa;

use std::{
    collections::HashSet,
    io::{self, BufWriter, Write},
};
use crate::ir::{
    isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, StoredBinaryOpcode, Value},
    visit::{self, IrVisitor},
    IrStore,
};
use self::isa::{F1Opcode, F2Opcode, F3Opcode, Instruction, Register};

pub fn gen_code<W: Write>(mut ir: IrStore, mut writer: BufWriter<W>) {
    // for each body:
        // traverse the cfg for the body, generating instructions for each bb in order

    // but for now, just main
    let body = ir.main_body().unwrap();
    let mut gen = DlxCodegen::new(&body, &mut writer);
    gen.visit_body(&body);
}


// my simple crappy attempt at register allocation for now:
    // R27 is the condition register -- condition results get stored there
    // for each value V, R(V) is its designated register
    // yes, this means you can only have 26 values total in the program
        // this will be fixed later
struct DlxCodegen<'b, 'wr, W: Write> {
    body: &'b Body,
    writer: &'wr mut BufWriter<W>,
    visited: HashSet<BasicBlock>,
}

impl<'b, 'wr, W: Write> DlxCodegen<'b, 'wr, W> {
    pub fn new(body: &'b Body, writer: &'wr mut BufWriter<W>) -> DlxCodegen<'b, 'wr, W> {
        DlxCodegen { body, writer, visited: HashSet::new() }
    }

    pub fn emit_instr(&mut self, instr: Instruction) {
        self.writer.write_all(instr.as_bytes().as_ref());
    }

    pub fn reg_for_val(&self, val: Value) -> Register {
        Register((val.0 + 1) as u8)
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
        if !self.visited.contains(&bb) {
            self.visited.insert(bb);
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

    fn visit_branch_instr(&mut self, _opcode: BranchOpcode, _dest: BasicBlock) {
        todo!();
    }

    fn visit_call_instr(&mut self, _func: &str) {
        todo!();
    }

    fn visit_cmp_instr(&mut self, lhs: Value, rhs: Value) {
        self.emit_instr(Instruction::F2(
            F2Opcode::Cmp,
            Register::RCMP,
            self.reg_for_val(lhs),
            self.reg_for_val(rhs),
        ));
    }

    fn visit_const_instr(&mut self, const_val: u32, dest: Value) {
        self.emit_instr(Instruction::F1(F1Opcode::Addi, self.reg_for_val(dest), Register::R0, const_val as u16));
    }

    fn visit_end_instr(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::R0));
    }

    fn visit_mu_instr(&mut self, _val: Value, _loc: CCLocation) {
        todo!();
    }

    fn visit_read_instr(&mut self, _dest: Value) {
        todo!();
    }

    fn visit_return_instr(&mut self) {
        todo!();
    }

    fn visit_stored_binop_instr(&mut self, _opcode: StoredBinaryOpcode, _src1: Value, _src2: Value, _dest: Value) {
        todo!();
    }

    fn visit_write_instr(&mut self, src: Value) {
        self.emit_instr(Instruction::F2(F2Opcode::Wrd, Register::R0, self.reg_for_val(src), Register::R0));
    }

    fn visit_writeln_instr(&mut self) {
        self.emit_instr(Instruction::F1(F1Opcode::Wrl, Register::R0, Register::R0, 0));
    }
}
