use std::{
    collections::{HashMap},
    io::{BufWriter, Write},
};
use crate::ir::{
    isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, ControlFlowEdge, StoredBinaryOpcode, Value},
    visit::{self, IrVisitor},
    IrStore,
};
use dlx::isa::{F1Opcode, F2Opcode, Instruction, Register};

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
    // for each value V, R(V) is its designated register
    // yes, this means you can only have 27 values total in the program
        // this will be fixed later
struct DlxCodegen<'b> {
    body: &'b Body,
    buffer: Vec<Instruction>,
    labels: HashMap<BasicBlock, i16>, // labels bb number to addr of first instruction
    next_instr_addr: i16, // addr in words of next instr
    unresolved_branches: Vec<UnresolvedBranch>,
    cutoff_point: Option<BasicBlock>,
    known_consts: HashMap<Value, u32>,
}

impl<'b> DlxCodegen<'b> {
    pub fn new(body: &'b Body) -> DlxCodegen<'b> {
        DlxCodegen {
            body,
            buffer: Vec::new(),
            labels: HashMap::new(),
            next_instr_addr: 0,
            unresolved_branches: Vec::new(),
            cutoff_point: None,
            known_consts: HashMap::new(),
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

    fn label_for(&self, bb: BasicBlock) -> Option<i16> {
        self.labels.get(&bb).copied()
    }

    fn branch_offset(&self, src_addr: i16, dest: BasicBlock) -> Option<i16> {
        self.label_for(dest).map(|l| l - src_addr)
    }

    fn reg_for_val(&self, val: Value) -> Register {
        Register((val.0 + 1) as u8)
    }

    fn mark_unresolved_branch(&mut self, ip: usize, addr: i16, dest: BasicBlock) {
        self.unresolved_branches.push(UnresolvedBranch { ip, addr, dest });
    }

    fn resolve_branches(&mut self) {
        for unresolved_br in self.unresolved_branches.iter() {
            let offset = self.branch_offset(
                unresolved_br.addr,
                unresolved_br.dest,
            ).expect("missing label for branch instr");

            match self.buffer[unresolved_br.ip] {
                Instruction::F1(opcode, _, _, ref mut dest) if opcode.is_branch() => *dest = offset,
                _ => unreachable!(),
            }
        }
    }

    fn load_and_visit_basic_block(&mut self, bb: BasicBlock) {
        self.visit_basic_block(bb, self.body.basic_block_data(bb));
    }

    fn imm_operands(
        &self,
        opcode: StoredBinaryOpcode,
        src1: Value,
        src2: Value,
    ) -> Option<(F1Opcode, Value, i16)> {
        let opcode = F1Opcode::try_from(opcode);

        match opcode {
            Ok(opcode @ F1Opcode::Addi) | Ok(opcode @ F1Opcode::Muli) => self.const_for_val(src1)
                .map(|imm| (src2, imm as i16))
                .or_else(|| self.const_for_val(src2).map(|imm| (src1, imm as i16)))
                .map(|(src, imm)| (opcode, src, imm)),

            Ok(opcode @ F1Opcode::Subi)
             | Ok(opcode @ F1Opcode::Divi)
             | Ok(opcode @ F1Opcode::Cmpi) => self.const_for_val(src2).map(|imm| (opcode, src1, imm as i16)),

            _ => None,
        }
    }

    fn mark_const(&mut self, val: Value, const_val: u32) {
        self.known_consts.insert(val, const_val);
    }

    fn const_for_val(&self, val: Value) -> Option<u32> {
        self.known_consts.get(&val).copied()
    }
}

impl IrVisitor for DlxCodegen<'_> {
    fn visit_body(&mut self, body: &Body) {
        if let Some((root, root_bb)) = body.root_block_data() {
            self.visit_basic_block(root, root_bb);
        }

        self.resolve_branches();
    }

    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        // emit label for basic block
        if Some(bb) != self.cutoff_point && !self.labels.contains_key(&bb) {
            self.mark_label(bb);
            visit::walk_basic_block(self, bb_data);

            match bb_data.edge() {
                ControlFlowEdge::Leaf => (),
                ControlFlowEdge::Fallthrough(dest) => {
                    self.load_and_visit_basic_block(dest);
                },
                ControlFlowEdge::Branch(dest) => {
                    self.load_and_visit_basic_block(dest);
                },
                ControlFlowEdge::IfStmt(then_bb, Some(else_bb), join_bb) => {
                    // ONLY IF ELSE BLOCK
                        // IF NO ELSE BLOCK, NO NEED FOR ANYTHING FANCY
                    // save prev cutoff point
                    let prev_cutoff_point = self.cutoff_point;

                    // set cutoff point to join block
                    self.cutoff_point = Some(join_bb);

                    // visit then block
                    self.load_and_visit_basic_block(then_bb);
                    // restore prev cutoff point
                        // this will implicitly remove the cutoff point if there wasn't a prev
                    self.cutoff_point = prev_cutoff_point;

                    // visit else block
                    self.load_and_visit_basic_block(else_bb);
                },
                ControlFlowEdge::IfStmt(then_bb, None, _) => {
                    // visit then block, which will implicitly visit join block
                    self.load_and_visit_basic_block(then_bb);
                },
                ControlFlowEdge::Loop(body_bb, follow_bb) => {
                    // visit body and follow blocks
                    self.load_and_visit_basic_block(body_bb);
                    self.load_and_visit_basic_block(follow_bb);
                }
            }
        }
    }

    fn visit_branch_instr(&mut self, opcode: BranchOpcode, cmp: Value, dest: BasicBlock) {
        let offset = self.branch_offset(self.next_instr_addr, dest);

        self.emit_instr(Instruction::F1(
            F1Opcode::from(opcode),
            self.reg_for_val(cmp),
            Register::R0,
            offset.unwrap_or(0), // either encode branch or fill with temporary value
        ));

        if offset.is_none() {
            self.mark_unresolved_branch(self.buffer.len() - 1, self.next_instr_addr - 1, dest);
        }
    }

    fn visit_call_instr(&mut self, _func: &str) {
        todo!();
    }

    fn visit_const_instr(&mut self, const_val: u32, dest: Value) {
        self.mark_const(dest, const_val);
        self.emit_instr(Instruction::F1(F1Opcode::Addi, self.reg_for_val(dest), Register::R0, const_val as i16));
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
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::RRET));
    }

    fn visit_stored_binop_instr(&mut self, opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value) {
        if matches!(opcode, StoredBinaryOpcode::Phi) {
            return; // FIXME: worry about this later
        }

        match self.imm_operands(opcode, src1, src2) {
            Some((opcode, src, imm)) => self.emit_instr(Instruction::F1(
                opcode,
                self.reg_for_val(dest),
                self.reg_for_val(src),
                imm,
            )),

            None => self.emit_instr(Instruction::F2(
                F2Opcode::from(opcode),
                self.reg_for_val(dest),
                self.reg_for_val(src1),
                self.reg_for_val(src2),
            )),
        }
    }

    fn visit_write_instr(&mut self, src: Value) {
        self.emit_instr(Instruction::F2(F2Opcode::Wrd, Register::R0, self.reg_for_val(src), Register::R0));
    }

    fn visit_writeln_instr(&mut self) {
        self.emit_instr(Instruction::F1(F1Opcode::Wrl, Register::R0, Register::R0, 0));
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
struct UnresolvedBranch {
    pub ip: usize,
    pub addr: i16,
    pub dest: BasicBlock,
}
