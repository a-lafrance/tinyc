#[macro_use] mod utils;

use std::{
    collections::{HashMap, HashSet},
    io::{BufWriter, Write},
};
use dlx::isa::{F1Opcode, F2Opcode, F3Opcode, Instruction, Register};
use crate::{
    driver::opt::OptConfig,
    ir::{
        isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, ControlFlowEdge, StoredBinaryOpcode, Value},
        visit::{self, IrVisitor},
        IrStore,
    },
    regalloc::{Location, LocationTable, RegisterSet},
};
use self::utils::UnresolvedBranch;

pub fn gen_code<W: Write>(mut ir: IrStore, mut writer: BufWriter<W>, opt: OptConfig) {
    // visit main body
    let body = ir.pop_main_body().expect("invariant violated: program must have main body");
    let mut gen = DlxCodegen::new(&body, opt);
    gen.visit_body(&body);

    // visit every other body
    for (_, body) in ir.into_bodies() {

    }

    for instr in gen.into_buffer().into_iter() {
        writer.write_all(instr.as_bytes().as_ref()).expect("failed to write instr");
    }
}


// super simple calling conventions:
    // typical call site:
        // mov R0, R20
        // psh R21 <- save R21 by pushing it on the stack
        // mov R6, R21
        // ... other params
        // psh R15
        // ... other params on stack
        // call f
        // pop R0 <- pop into nowhere
        // ... pop params off stack
        // pop R21 <- load old value back into R21
// To handle function calls:

type DlxLocation = Location<Register>;

struct DlxCodegen<'b> {
    body: &'b Body,
    buffer: Vec<Instruction>,
    loc_table: LocationTable<Register>,
    labels: HashMap<BasicBlock, i16>, // labels bb number to addr of first instruction
    reg_live_set: HashSet<Register>,
    known_consts: HashMap<Value, u32>,
    current_stack_args: Vec<Value>,
    next_instr_addr: i16, // addr in words of next instr
    unresolved_branches: Vec<UnresolvedBranch>,
    cutoff_point: Option<BasicBlock>,
    opt: OptConfig,
}

impl<'b> DlxCodegen<'b> {
    pub const STACK_TMP1: Register = Register(25);
    pub const STACK_TMP2: Register = Register(26);
    const FP_OFFSET: i16 = -4;

    pub fn new(body: &'b Body, opt: OptConfig) -> DlxCodegen<'b> {
        DlxCodegen {
            body,
            buffer: Vec::with_capacity(64),
            loc_table: LocationTable::alloc_from(body),
            labels: HashMap::default(),
            reg_live_set: HashSet::default(),
            known_consts: HashMap::default(),
            current_stack_args: Vec::with_capacity(8),
            next_instr_addr: 0,
            unresolved_branches: Vec::with_capacity(16),
            cutoff_point: None,
            opt,
        }
    }

    pub fn into_buffer(self) -> Vec<Instruction> {
        self.buffer
    }

    fn emit_instr(&mut self, instr: Instruction) {
        self.next_instr_addr += 1;
        self.buffer.push(instr);
    }

    fn emit_load(&mut self, dest: Register, base: Register, offset: i16) {
        self.emit_instr(Instruction::F1(F1Opcode::Ldw, dest, base, offset));
    }

    fn emit_store(&mut self, src: Register, base: Register, offset: i16) {
        self.emit_instr(Instruction::F1(F1Opcode::Stw, src, base, offset));
    }

    fn emit_push(&mut self, src: Register) {
        self.emit_instr(Instruction::F1(F1Opcode::Psh, src, Register::RSP, Self::FP_OFFSET));
    }

    fn emit_pop(&mut self, dest: Register) {
        self.emit_instr(Instruction::F1(F1Opcode::Psh, dest, Register::RSP, Self::FP_OFFSET));
    }

    fn emit_reg_to_reg_move(&mut self, src: Register, dest: Register) {
        self.emit_instr(Instruction::F1(F1Opcode::Addi, dest, src, 0));
    }

    fn emit_loc_to_loc_move(&mut self, src: DlxLocation, dest: DlxLocation) {
        match src {
            Location::Reg(src_reg) => match dest {
                Location::Reg(dest_reg) => self.emit_reg_to_reg_move(src_reg, dest_reg),
                Location::Stack(dest_offset) => self.emit_store(src_reg, Register::RFP, dest_offset as i16),
            }

            Location::Stack(src_offset) => match dest {
                Location::Reg(dest_reg) => self.emit_load(dest_reg, Register::RFP, src_offset as i16),
                Location::Stack(dest_offset) => {
                    self.emit_load(Self::STACK_TMP1, Register::RFP, src_offset as i16);
                    self.emit_store(Self::STACK_TMP1, Register::RFP, dest_offset as i16);
                }
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::RRET));
    }

    fn emit_prologue(&mut self) {
        // Only emit a prologue if the stack is actually used
        if self.loc_table.stack_in_use() {
            // Push old fp onto the stack and update it
            self.emit_push(Register::RFP);
            self.emit_reg_to_reg_move(Register::RSP, Register::RFP);

            let total_local_offset = self.loc_table.total_local_offset() as i16;

            if total_local_offset > 0 {
                // If locals are allocated on the stack, move the stack pointer up
                self.emit_instr(Instruction::F1(
                    F1Opcode::Addi,
                    Register::RSP,
                    Register::RSP,
                    total_local_offset
                ));
            }
        }
    }

    fn emit_epilogue(&mut self) {
        // Only emit anything if the stack is actually used
        if self.loc_table.stack_in_use() {
            let total_local_offset = self.loc_table.total_local_offset() as i16;

            if total_local_offset > 0 {
                // If locals are allocated on the stack, move the stack pointer back down
                self.emit_instr(Instruction::F1(
                    F1Opcode::Subi,
                    Register::RSP,
                    Register::RSP,
                    total_local_offset
                ));
            }

            // Pop old fp off the stack and return
            self.emit_pop(Register::RFP);
            self.emit_return();
        }
    }

    fn emit_push_stack_args(&mut self) {
        // FIXME: i feel like i should be able to avoid this clone
        for arg in self.current_stack_args.clone().iter().copied() {
            // force arg into register and push onto stack
            let src_reg = self.reg_for_val(arg, Self::STACK_TMP1);
            self.emit_push(src_reg);
        }
    }

    fn emit_pop_stack_args(&mut self) {
        for _ in 0..self.current_stack_args.len() {
            self.emit_pop(Self::STACK_TMP1); // pop into useless register (idk if you can pop into nowhere)
        }

        self.current_stack_args.clear();
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

    fn loc_for_val(&self, val: Value) -> DlxLocation {
        self.loc_table.get(val).expect("invariant violated: missing location for value")
    }

    fn cc_location(&self, loc: CCLocation) -> DlxLocation {
        self.loc_table.get_from_cc(loc).expect("invariant violated: missing location for calling convention")
    }

    fn reg_for_val(&mut self, val: Value, dest_reg: Register) -> Register {
        let reg = match self.loc_for_val(val) {
            Location::Reg(r) => r,
            Location::Stack(offset) => {
                self.emit_load(dest_reg, Register::RFP, self.stack_offset(offset as i16));
                dest_reg
            }
        };

        self.mark_reg_in_use(reg);
        reg
    }

    fn mark_reg_in_use(&mut self, reg: Register) {
        self.reg_live_set.insert(reg);
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
        if !self.opt.instr_select {
            return None;
        }

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

    fn stack_offset(&self, offset: i16) -> i16 {
        offset * -4
    }

    fn push_stack_arg(&mut self, arg: Value) {
        self.current_stack_args.push(arg);
    }
}

impl IrVisitor for DlxCodegen<'_> {
    fn visit_body(&mut self, body: &Body) {
        if let Some((root, root_bb)) = body.root_block_entry() {
            self.emit_prologue();
            self.visit_basic_block(root, root_bb);
            self.resolve_branches();
            self.emit_epilogue();
        }
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

    fn visit_bind_instr(&mut self, dest: Value, cc_loc: CCLocation) {
        // when you hit a ret val bind instruction:
            // if the location of the dest value isn't already the ret val register:
                // if the location is a register, emit reg_to_reg move from ret val register
                // if the location is on the stack, emit a store onto the stack
        // when you hit an arg bind instruction:
            // if dest location isn't the right register, move out of the register into the dest location
                // see ret val bind above
        let src_loc = self.cc_location(cc_loc);
        let dest_loc = self.loc_for_val(dest);

        if src_loc != dest_loc {
            // move out of loc into dest loc
            self.emit_loc_to_loc_move(src_loc, dest_loc);
        }
    }

    fn visit_branch_instr(&mut self, opcode: BranchOpcode, cmp: Value, dest: BasicBlock) {
        let offset = self.branch_offset(self.next_instr_addr, dest);
        let cmp_reg = self.reg_for_val(cmp, Self::STACK_TMP1);

        self.emit_instr(Instruction::F1(
            F1Opcode::from(opcode),
            cmp_reg,
            Register::R0,
            offset.unwrap_or(0), // either encode branch or fill with temporary value
        ));

        if offset.is_none() {
            self.mark_unresolved_branch(self.buffer.len() - 1, self.next_instr_addr - 1, dest);
        }
    }

    fn visit_call_instr(&mut self, _func: &str) {
        self.emit_push_stack_args();
        self.emit_instr(Instruction::F3(F3Opcode::Jsr, todo!()));
        self.emit_pop_stack_args();
        // pop saved registers
    }

    fn visit_const_instr(&mut self, const_val: u32, dest: Value) {
        do_with_store!(self, dest => dest_reg, {
            self.mark_const(dest, const_val);
            self.emit_instr(Instruction::F1(F1Opcode::Addi, dest_reg, Register::R0, const_val as i16));
        });
    }

    fn visit_end_instr(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::R0));
    }

    fn visit_move_instr(&mut self, src: Value, loc: CCLocation) {
        // whenever you hit a mov instruction to an arg location:
            // if register exists for arg location:
                // issue a load or move from src value into arg register
                // meaning either reg_to_reg move or load from stack directly
                    // alternatively, if the locations match don't even emit anything
            // else:
                // push value onto stack
                // actually, keep a running list of params that need to be pushed onto the stack
        // when you hit a ret val move instruction:
            // do the same as ret val bind, except from src value location into ret val register
        let src_loc = self.loc_for_val(src);

        match loc {
            CCLocation::Arg(_) => match Register::for_cc_location(loc) {
                Some(r) => self.emit_loc_to_loc_move(src_loc, Location::Reg(r)),
                None => self.push_stack_arg(src),
            }

            CCLocation::RetVal => self.emit_loc_to_loc_move(src_loc, self.cc_location(loc)),
        }
    }

    fn visit_read_instr(&mut self, dest: Value) {
        do_with_store!(self, dest => dest_reg, {
            self.emit_instr(Instruction::F2(F2Opcode::Rdd, dest_reg, Register::R0, Register::R0));
        });
    }

    fn visit_return_instr(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::RRET));
    }

    fn visit_stored_binop_instr(&mut self, opcode: StoredBinaryOpcode, src1: Value, src2: Value, dest: Value) {
        if matches!(opcode, StoredBinaryOpcode::Phi) {
            return; // FIXME: worry about this later
        }

        do_with_store!(self, dest => dest_reg, {
            match self.imm_operands(opcode, src1, src2) {
                Some((opcode, src, imm)) => {
                    let src_reg = self.reg_for_val(src, Self::STACK_TMP1);

                    self.emit_instr(Instruction::F1(
                        opcode,
                        dest_reg,
                        src_reg,
                        imm,
                    ));
                },

                None => {
                    let src1_reg = self.reg_for_val(src1, Self::STACK_TMP1);
                    let src2_reg = self.reg_for_val(src2, Self::STACK_TMP2);

                    self.emit_instr(Instruction::F2(
                        F2Opcode::from(opcode),
                        dest_reg,
                        src1_reg,
                        src2_reg,
                    ));
                },
            }
        });
    }

    fn visit_write_instr(&mut self, src: Value) {
        let src_reg = self.reg_for_val(src, Self::STACK_TMP1);

        self.mark_reg_in_use(src_reg);
        self.emit_instr(Instruction::F2(F2Opcode::Wrd, Register::R0, src_reg, Register::R0));
    }

    fn visit_writeln_instr(&mut self) {
        self.emit_instr(Instruction::F1(F1Opcode::Wrl, Register::R0, Register::R0, 0));
    }
}
