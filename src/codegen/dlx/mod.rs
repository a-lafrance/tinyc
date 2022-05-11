mod isa;

use std::{
    collections::{HashMap, HashSet},
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
    let mut gen = DlxCodegen::new(&body);
    gen.visit_body(&body);

    for instr in gen.into_buffer().into_iter() {
        writer.write_all(instr.as_bytes().as_ref()).expect("failed to write instr");
    }
}


// my simple crappy attempt at register allocation for now:
    // R27 is the condition register -- condition results get stored there
    // for each value V, R(V) is its designated register
    // yes, this means you can only have 26 values total in the program
        // this will be fixed later
struct DlxCodegen<'b> {
    body: &'b Body,
    buffer: Vec<Instruction>,
    labels: HashMap<BasicBlock, u16>, // labels bb number to addr of first instruction
    next_instr_addr: u16, // addr in words of next instr
    current_branch_ip: Option<(usize, u16, BasicBlock)>, // index, addr of current branch instruction
}

impl<'b> DlxCodegen<'b> {
    pub fn new(body: &'b Body) -> DlxCodegen<'b> {
        DlxCodegen {
            body,
            buffer: Vec::new(),
            labels: HashMap::new(),
            next_instr_addr: 0,
            current_branch_ip: None,
        }
    }

    pub fn into_buffer(self) -> Vec<Instruction> {
        self.buffer
    }

    fn emit_instr(&mut self, instr: Instruction) {
        self.next_instr_addr += 1;
        self.buffer.push(instr);
    }

    fn mark_label(&mut self, bb: BasicBlock) {
        self.labels.insert(bb, self.next_instr_addr);
    }

    fn label_for(&self, bb: BasicBlock) -> Option<u16> {
        self.labels.get(&bb).copied()
    }

    fn branch_offset(&self, src_addr: u16, dest: BasicBlock) -> Option<u16> {
        self.label_for(dest).map(|l| l - src_addr )
    }

    fn reg_for_val(&self, val: Value) -> Register {
        Register((val.0 + 1) as u8)
    }
}

impl IrVisitor for DlxCodegen<'_> {
    fn visit_body(&mut self, body: &Body) {
        if let Some((root, root_bb)) = body.root_block_data() {
            self.visit_basic_block(root, root_bb);
        }
    }

    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        // emit label for basic block
        if !self.labels.contains_key(&bb) {
            self.mark_label(bb);

            visit::walk_basic_block(self, bb_data);
            let branch_ip = self.current_branch_ip;

            if let Some(ft_dest) = bb_data.fallthrough_dest() {
                let ft_dest_data = self.body.basic_block_data(ft_dest);
                self.visit_basic_block(ft_dest, ft_dest_data);
            }

            if let Some(br_dest) = bb_data.branch_dest() {
                let br_dest_data = self.body.basic_block_data(br_dest);
                self.visit_basic_block(br_dest, br_dest_data);
            }

            // go back and modify branch instruction
            if let Some((branch_ip, branch_addr, branch_dest)) = branch_ip {
                let offset = self.branch_offset(
                    branch_addr,
                    branch_dest,
                ).expect("missing label for branch instr");

                match self.buffer[branch_ip] {
                    Instruction::F1(opcode, _, _, ref mut dest) if opcode.is_branch() => *dest = offset,
                    _ => unreachable!(),
                }
            }
        }
    }

    fn visit_branch_instr(&mut self, opcode: BranchOpcode, dest: BasicBlock) {
        let comparator = match opcode {
            BranchOpcode::Br => Register::R0,
            _ => Register::RCMP,
        };

        self.emit_instr(Instruction::F1(
            F1Opcode::from(opcode),
            comparator,
            Register::R0,
            self.branch_offset(self.next_instr_addr, dest).unwrap_or(0), // either encode branch or fill with temporary value
        ));
        self.current_branch_ip = Some((self.buffer.len() - 1, self.next_instr_addr - 1, dest));
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
        // TODO: may use immediate instructions instead of actually allocating constants
        self.emit_instr(Instruction::F1(F1Opcode::Addi, self.reg_for_val(dest), Register::R0, const_val as u16));
    }

    fn visit_end_instr(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::R0));
    }

    fn visit_mu_instr(&mut self, _val: Value, _loc: CCLocation) {
        todo!();
    }

    fn visit_read_instr(&mut self, dest: Value) {
        self.emit_instr(Instruction::F2(F2Opcode::Rdd, self.reg_for_val(dest), Register::R0, Register::R0));
    }

    fn visit_return_instr(&mut self) {
        todo!();
    }

    fn visit_stored_binop_instr(&mut self, opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value) {
        if matches!(opcode, StoredBinaryOpcode::Phi) {
            return; // FIXME: worry about this later
        }

        self.emit_instr(Instruction::F2(
            F2Opcode::from(opcode),
            self.reg_for_val(dest),
            self.reg_for_val(src1),
            self.reg_for_val(src2),
        ));
    }

    fn visit_write_instr(&mut self, src: Value) {
        self.emit_instr(Instruction::F2(F2Opcode::Wrd, Register::R0, self.reg_for_val(src), Register::R0));
    }

    fn visit_writeln_instr(&mut self) {
        self.emit_instr(Instruction::F1(F1Opcode::Wrl, Register::R0, Register::R0, 0));
    }
}
