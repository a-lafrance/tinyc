use super::isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, Instruction, StoredBinaryOpcode, Value};

pub trait IrVisitor: Sized {
    fn visit_body(&mut self, _body: &Body) { }
    fn visit_basic_block(&mut self, _bb: BasicBlock, bb_data: &BasicBlockData) {
        walk_basic_block(self, bb_data);
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        walk_instr(self, instr);
    }

    fn visit_branch_instr(&mut self, _opcode: BranchOpcode, _cmp: Value, _dest: BasicBlock) { }
    fn visit_call_instr(&mut self, _func: &str) { }
    fn visit_const_instr(&mut self, _const_val: u32, _dest: Value) { }
    fn visit_end_instr(&mut self) { }
    fn visit_mu_instr(&mut self, _val: Value, _loc: CCLocation) { }
    fn visit_nop_instr(&mut self) { }
    fn visit_read_instr(&mut self, _dest: Value) { }
    fn visit_return_instr(&mut self) { }
    fn visit_stored_binop_instr(&mut self, _opcode: StoredBinaryOpcode, _src1: Value, _src2: Value, _dest: Value) { }
    fn visit_write_instr(&mut self, _src: Value) { }
    fn visit_writeln_instr(&mut self) { }
    fn visit_unconditional_branch_instr(&mut self, _dest: BasicBlock) { }
}

pub fn walk_basic_block(v: &mut impl IrVisitor, bb: &BasicBlockData) {
    for instr in bb.body().iter() {
        v.visit_instr(instr);
    }
}

pub fn walk_instr(v: &mut impl IrVisitor, instr: &Instruction) {
    match instr {
        Instruction::Branch(opcode, cmp, dest) => v.visit_branch_instr(*opcode, *cmp, *dest),
        Instruction::Call(func) => v.visit_call_instr(func),
        Instruction::Const(const_val, dest) => v.visit_const_instr(*const_val, *dest),
        Instruction::End => v.visit_end_instr(),
        Instruction::Mu(val, loc) => v.visit_mu_instr(*val, *loc),
        Instruction::Nop => v.visit_nop_instr(),
        Instruction::Read(dest) => v.visit_read_instr(*dest),
        Instruction::Return => v.visit_return_instr(),
        Instruction::StoredBinaryOp(opcode, src1, src2, dest) => v.visit_stored_binop_instr(*opcode, *src1, *src2, *dest),
        Instruction::Write(src) => v.visit_write_instr(*src),
        Instruction::Writeln => v.visit_writeln_instr(),
        Instruction::UnconditionalBranch(dest) => v.visit_unconditional_branch_instr(*dest),
    }
}
