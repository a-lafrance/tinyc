use super::isa::{BasicBlock, BasicBlockData, BranchOpcode, InstrData, StoredBinaryOpcode, Value};

pub trait IrVisitor: Sized {
    fn visit_basic_block(&mut self, bb: &BasicBlockData) {
        walk_basic_block(self, bb);
    }

    fn visit_instr(&mut self, instr: &InstrData) {
        walk_instr(self, instr);
    }

    fn visit_branch_instr(&mut self, _opcode: BranchOpcode, _dest: BasicBlock) { }
    fn visit_cmp_instr(&mut self, _lhs: Value, _rhs: Value) { }
    fn visit_const_instr(&mut self, _const_val: u32, _dest: Value) { }
    fn visit_end_instr(&mut self) { }
    fn visit_nop_instr(&mut self) { }
    fn visit_read_instr(&mut self, _dest: Value) { }
    fn visit_stored_binop_instr(&mut self, _opcode: StoredBinaryOpcode, _src1: Value, _src2: Value, _dest: Value) { }
    fn visit_write_instr(&mut self, _src: Value) { }
    fn visit_writeln_instr(&mut self) { }
}

pub fn walk_basic_block(v: &mut impl IrVisitor, bb: &BasicBlockData) {
    todo!();
}

pub fn walk_instr(v: &mut impl IrVisitor, instr: &InstrData) {
    match instr {
        InstrData::Branch(opcode, dest) => v.visit_branch_instr(*opcode, *dest),
        InstrData::Cmp(lhs, rhs) => v.visit_cmp_instr(*lhs, *rhs),
        InstrData::Const(const_val, dest) => v.visit_const_instr(*const_val, *dest),
        InstrData::End => v.visit_end_instr(),
        InstrData::Nop => v.visit_nop_instr(),
        InstrData::Read(dest) => v.visit_read_instr(*dest),
        InstrData::StoredBinaryOp { opcode, src1, src2, dest } => v.visit_stored_binop_instr(*opcode, *src1, *src2, *dest),
        InstrData::Write(src) => v.visit_write_instr(*src),
        InstrData::Writeln => v.visit_writeln_instr(),
    }
}
