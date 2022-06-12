#[macro_use] mod utils;

use std::{
    collections::{HashMap, HashSet},
    io::{BufWriter, Write},
};
use dlx::isa::{F1Opcode, F2Opcode, F3Opcode, Instruction, Register};
use crate::{
    driver::opt::{OptConfig, RegAllocator},
    ir::{
        isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, ControlFlowEdge, StoredBinaryOpcode, Value},
        visit::{self, IrVisitor},
        IrStore,
    },
    regalloc::{color::ColoringAllocator, simple::SimpleAllocator, Location, LocationTable, RegisterSet},
    utils::Keyword,
};
use self::utils::{UnresolvedBranch, UnresolvedCall};

pub fn gen_code<W: Write>(ir: IrStore, writer: W, opt: OptConfig) {
    let mut writer = BufWriter::new(writer);

    for instr in run_generator(ir, opt).into_iter() {
        writer.write_all(instr.to_bytes().as_ref()).expect("failed to write instr");
    }
}

pub fn gen_asm<W: Write>(ir: IrStore, writer: W, opt: OptConfig) {
    let mut writer = BufWriter::new(writer);

    for instr in run_generator(ir, opt).into_iter() {
        writeln!(writer, "{}", instr).expect("failed to dump asm instr");
    }
}

fn run_generator(ir: IrStore, opt: OptConfig) -> Vec<Instruction> {
    DlxCodegen::gen_from_ir(ir, opt)
}

// still todo:
    // saving registers
    // need to do the whole jump to epilogue instead of just returning thing


type DlxLocation = Location<Register>;

struct DlxCodegen<'b> {
    current_context: BodyContext<'b>,
    buffer: Vec<Instruction>,
    body_labels: HashMap<String, usize>,
    current_stack_args: Vec<Value>,
    next_instr_addr: usize, // word-aligned addr of next instruction
    unresolved_branches: Vec<UnresolvedBranch>,
    unresolved_calls: Vec<UnresolvedCall>,
    cutoff_point: Option<BasicBlock>,
    opt: OptConfig,
}

impl<'b> DlxCodegen<'b> {
    pub const STACK_TMP1: Register = Register(25);
    pub const STACK_TMP2: Register = Register(26);
    const FP_OFFSET: i16 = -4;

    pub fn gen_from_ir(mut ir: IrStore, opt: OptConfig) -> Vec<Instruction> {
        // visit main body
        let main_body = ir.pop_main_body().expect("invariant violated: program must have main body");
        let mut gen = DlxCodegen::from_main(&main_body, opt);

        // visit every other body
        for (name, body) in ir.bodies() {
            gen.visit_named_body(name.to_string(), body);
        }

        gen.resolve_calls();
        gen.into_buffer()
    }

    pub fn from_main(main_body: &'b Body, opt: OptConfig) -> DlxCodegen<'b> {
        let mut gen = DlxCodegen {
            current_context: BodyContext::from(main_body, opt),
            buffer: Vec::with_capacity(64),
            body_labels: HashMap::default(),
            current_stack_args: Vec::with_capacity(8),
            next_instr_addr: 0,
            unresolved_branches: Vec::with_capacity(32),
            unresolved_calls: Vec::with_capacity(32),
            cutoff_point: None,
            opt,
        };

        gen.visit_named_body(Keyword::Main.to_string(), main_body);
        gen
    }

    pub fn into_buffer(self) -> Vec<Instruction> {
        self.buffer
    }

    pub fn visit_named_body(&mut self, name: String, body: &'b Body) {
        self.current_context = BodyContext::from(body, self.opt);
        self.insert_body_label(name);
        self.visit_body(body);
    }

    fn current_body(&self) -> &'b Body {
        self.current_context.body
    }

    fn insert_body_label(&mut self, name: String) {
        self.body_labels.insert(name, self.next_instr_addr as usize);
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
        self.emit_instr(Instruction::F1(F1Opcode::Pop, dest, Register::RSP, Self::FP_OFFSET));
    }

    fn emit_branch(&mut self, opcode: F1Opcode, cmp_reg: Register, dest: BasicBlock) {
        let offset = self.branch_offset(self.next_instr_addr, dest);

        self.emit_instr(Instruction::F1(
            opcode,
            cmp_reg,
            Register::R0,
            offset.unwrap_or(0), // either encode branch or fill with temporary value
        ));

        if offset.is_none() {
            // FIXME: don't like that there are magic constants here
            self.mark_unresolved_branch(
                self.buffer.len() - 1,
                self.next_instr_addr - 1,
                dest,
            );
        }
    }

    fn emit_unconditional_branch(&mut self, dest: BasicBlock) {
        self.emit_branch(F1Opcode::Beq, Register::R0, dest);
    }

    fn emit_reg_to_reg_move(&mut self, src: Register, dest: Register) {
        if src != dest {
            self.emit_instr(Instruction::F1(F1Opcode::Addi, dest, src, 0));
        }
    }

    fn emit_loc_to_loc_move(&mut self, src: DlxLocation, dest: DlxLocation) {
        if src != dest {
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
    }

    fn emit_return(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::RRET));
    }

    fn emit_prologue(&mut self) {
        // Only emit a prologue if the stack is actually used
        if self.current_context.loc_table.stack_in_use() {
            // Push old fp onto the stack and update it
            self.emit_push(Register::RFP);
            self.emit_reg_to_reg_move(Register::RSP, Register::RFP);

            let total_local_offset = self.stack_offset(self.current_context.loc_table.total_local_offset());

            if total_local_offset != 0 {
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
        // TODO: mark addr of epilogue
        // Only emit anything if the stack is actually used
        if self.current_context.loc_table.stack_in_use() {
            let total_local_offset = self.stack_offset(self.current_context.loc_table.total_local_offset());

            if total_local_offset != 0 {
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
        }

        self.emit_return();
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
        self.current_context.labels.insert(bb, self.next_instr_addr);
    }

    fn label_for(&self, bb: BasicBlock) -> Option<usize> {
        self.current_context.labels.get(&bb).copied()
    }

    fn label_for_body(&self, name: &str) -> Option<usize> {
        self.body_labels.get(name).copied()
    }

    fn branch_offset(&self, src_addr: usize, dest: BasicBlock) -> Option<i16> {
        self.label_for(dest)
            .map(|l| l - src_addr)
            .map(|o| dlx::utils::word_to_byte_addr(o) as i16)
    }

    fn loc_for_val(&self, val: Value) -> Option<DlxLocation> {
        self.current_context.loc_table.get(val)
    }

    fn expect_loc_for_val(&self, val: Value) -> DlxLocation {
        self.loc_for_val(val).expect("invariant violated: missing location for value")
    }

    fn cc_location(&self, loc: CCLocation) -> DlxLocation {
        self.current_context.loc_table.get_from_cc(loc).expect("invariant violated: missing location for calling convention")
    }

    fn reg_for_val(&mut self, val: Value, dest_reg: Register) -> Register {
        let reg = match self.expect_loc_for_val(val) {
            Location::Reg(r) => r,
            Location::Stack(offset) => {
                self.emit_load(dest_reg, Register::RFP, self.stack_offset(offset));
                dest_reg
            }
        };

        self.mark_reg_in_use(reg);
        reg
    }

    fn mark_reg_in_use(&mut self, reg: Register) {
        self.current_context.reg_live_set.insert(reg);
    }

    fn mark_unresolved_branch(&mut self, ip: usize, addr: usize, dest: BasicBlock) {
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

        self.unresolved_branches.clear();
    }

    fn mark_unresolved_call(&mut self, ip: usize, dest: String) {
        self.unresolved_calls.push(UnresolvedCall { ip, dest });
    }

    fn resolve_calls(&mut self) {
        for call in self.unresolved_calls.iter() {
            let dest_index = self.label_for_body(&call.dest).expect("missing label for body");
            let dest_addr = dlx::utils::word_to_byte_addr(dest_index);

            match self.buffer[call.ip] {
                Instruction::F3(F3Opcode::Jsr, ref mut dest) => *dest = dest_addr as i32,
                _ => unreachable!(),
            }
        }

        self.unresolved_calls.clear();
    }

    fn load_and_visit_basic_block(&mut self, bb: BasicBlock) {
        self.visit_basic_block(bb, self.current_body().basic_block_data(bb));
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

    fn mark_const(&mut self, val: Value, const_val: i32) {
        self.current_context.known_consts.insert(val, const_val);
    }

    fn const_for_val(&self, val: Value) -> Option<i32> {
        self.current_context.known_consts.get(&val).copied()
    }

    fn stack_offset(&self, index: isize) -> i16 {
        (index as i16 + 2) * -4
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
        if Some(bb) != self.cutoff_point && !self.current_context.labels.contains_key(&bb) {
            self.mark_label(bb);
            visit::walk_basic_block(self, bb_data);

            match bb_data.edge() {
                ControlFlowEdge::Leaf => {}

                ControlFlowEdge::Fallthrough(dest) => {
                    self.load_and_visit_basic_block(dest);
                }

                ControlFlowEdge::Branch(dest) => {
                    self.load_and_visit_basic_block(dest);
                }

                ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb) => {
                    // get a list of phis that need to be resolved from join bb, ie (dest, left, right)

                    // set cutoff point to join block
                    let prev_cutoff_point = self.cutoff_point;
                    self.cutoff_point = Some(join_bb);

                    // visit then block
                    self.load_and_visit_basic_block(then_bb);

                    // restore prev cutoff point, or erase cutoff point if there was no prev
                    self.cutoff_point = prev_cutoff_point;

                    // visit dest block
                    match else_bb {
                        Some(else_bb) => self.load_and_visit_basic_block(else_bb),
                        None => self.load_and_visit_basic_block(join_bb),
                    }
                }

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
        if let Some(dest_loc) = self.loc_for_val(dest) {
            let src_loc = self.cc_location(cc_loc);
            self.emit_loc_to_loc_move(src_loc, dest_loc);
        }
    }

    fn visit_branch_instr(&mut self, opcode: BranchOpcode, cmp: Value, dest: BasicBlock) {
        let cmp_reg = self.reg_for_val(cmp, Self::STACK_TMP1);
        self.emit_branch(F1Opcode::from(opcode), cmp_reg, dest);
    }

    fn visit_call_instr(&mut self, func: &str) {
        self.emit_push_stack_args();
        self.mark_unresolved_call(self.next_instr_addr as usize, func.to_string());
        self.emit_instr(Instruction::F3(F3Opcode::Jsr, 0));
        self.emit_pop_stack_args();
        // TODO: pop saved registers
    }

    fn visit_const_instr(&mut self, const_val: i32, dest: Value) {
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
        let src_loc = self.expect_loc_for_val(src);

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
        // actually, have to branch to epilogue instead

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

    fn visit_unconditional_branch_instr(&mut self, dest: BasicBlock) {
        self.emit_unconditional_branch(dest);
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


// NOTE: i'm accessing these fields directly all over the place; is that valid?
struct BodyContext<'b> {
    pub body: &'b Body,
    pub loc_table: LocationTable<Register>,
    pub labels: HashMap<BasicBlock, usize>, // labels bb number to addr of first instruction
    pub reg_live_set: HashSet<Register>,
    pub known_consts: HashMap<Value, i32>,
}

impl<'b> BodyContext<'b> {
    fn from(body: &'b Body, opt: OptConfig) -> BodyContext {
        BodyContext {
            body,
            loc_table: match opt.reg_alloc {
                RegAllocator::Simple => LocationTable::alloc_from::<SimpleAllocator>(body),
                RegAllocator::Coloring => LocationTable::alloc_from::<ColoringAllocator>(body),
            },
            labels: HashMap::default(),
            reg_live_set: HashSet::default(),
            known_consts: HashMap::default(),
        }
    }
}
