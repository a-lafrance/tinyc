#[macro_use] mod utils;

use std::{
    collections::{HashMap, HashSet},
    io::{BufWriter, Write},
};
use dlx::isa::{F1Opcode, F2Opcode, Instruction, Register};
use crate::{
    driver::opt::OptConfig,
    ir::{
        isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, ControlFlowEdge, StoredBinaryOpcode, Value},
        visit::{self, IrVisitor},
        IrStore,
    },
    regalloc::{Location, LocationTable},
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
    // R20 - R24 are reserved for parameter passing -- this means the first 5 params can be passed in registers
    // R25 - R26 are (naively) reserved as "load/store temporaries" for transferring values to/from the stack
    // R27 is the return value register, where the return value of a function should be stored
    // any more parameters must be passed on the stack in reverse order
    // the stack frame for a function should look like:
        /*
            -------- <- each line (with content) is a word
            params (as required)
            ...
            --------
            prev fp
            --------
            locals (as required)
            ...
            --------
        */
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
    // typical function:
        // prologue:
            // psh RFP
            // mov RSP -> RFP
            // addi RSP, N <- save space for locals
        // ... refer to locals on stack as required
        // ... refer to params on stack as required
        // IMPORTANT: you have to jump to the epilogue to return, rather than just returning
        // epilogue:
            // subi RSP, N <- shrink stack back down
            // pop RFP
            // ret
// calling convention impl plan:
    // impl ir pass that scans function body and determines its prologue and epilogue
        // pass must determine how much stack space must be allocated, assign param values to their registers/stack vars, etc
    // when you visit a function, run pass to determine pro/epilogue, then emit prologue, then emit epilogue after visit body
    // split mu instructions into 2 variants:
        // "bind" binds the value "out of" the register, i.e. it establishes that from this point forward they're linked
            // importantly this means you don't have to move anything into the register
            // use cases: link to params within functions, link to return val post-call
            // because in both cases, you want to "bind" the value to the register in an "out" fashion
        // "move" moves the value into the register
            // establishes that you have to funnel the value into the register (or keep it there in the first place)
            // use case: pass params to function
            // because you have to ensure that the value is moved into the register
        // difference is that binding establishes an "out-flow", where data flows out of that register that represents the value
            // but, moving establishes an "in-flow", where data must flow into the register at that instruction from the value
        // formally, in ir these look like:
            // `mov $0, ArgLoc0` -> move value $0 into the location of the 1st arg
            // `bind $3, RetValLoc` -> bind the value $3 to the location of the return value
    // given the 2 separate mu-type instructions, here's how to handle function calls:
        // whenever you visit a mov instruction, gather a running list of params for the current call
        // then, when you hit the call instruction, first emit a call prologue for the mov instructions
        // first, for each mov into a register:
            // if the register is in use (see above), push its value onto the stack
            // ...to be continued
// basically to do all this calling convention stuff:
    // when you start visiting an ir body:
        // run ir pass to check calling conventions
        // emit prologue
        // visit body as usual
        // emit epilogue
    // when you hit a move instruction:
        // keep a running list of params and put them in the right places before function call
        // if you need to save registers, do so on the stack
        // post-call if you had to save registers, pop them back off the stack
// to do this we'll need, among other things:
    // ir pass to check calling conventions
        // and logic to emit prologue/epilogue according to them
    // codegen logic for move/call instructions
    // extend the codegen system to multiple bodies
        // including a way to store the address of each function


struct DlxCodegen<'b> {
    body: &'b Body,
    buffer: Vec<Instruction>,
    loc_table: LocationTable<Register>,
    labels: HashMap<BasicBlock, i16>, // labels bb number to addr of first instruction
    reg_live_set: HashSet<Register>,
    known_consts: HashMap<Value, u32>,
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
            buffer: Vec::new(),
            loc_table: LocationTable::alloc_from(body),
            labels: HashMap::default(),
            reg_live_set: HashSet::default(),
            known_consts: HashMap::default(),
            next_instr_addr: 0,
            unresolved_branches: Vec::new(),
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

    fn emit_reg2reg_move(&mut self, src: Register, dest: Register) {
        self.emit_instr(Instruction::F1(F1Opcode::Addi, dest, src, 0));
    }

    fn emit_return(&mut self) {
        self.emit_instr(Instruction::F2(F2Opcode::Ret, Register::R0, Register::R0, Register::RRET));
    }

    fn emit_prologue(&mut self) {
        // Only emit a prologue if the stack is actually used
        if self.loc_table.stack_in_use() {
            // Push old fp onto the stack and update it
            self.emit_push(Register::RFP);
            self.emit_reg2reg_move(Register::RSP, Register::RFP);

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

    fn mark_label(&mut self, bb: BasicBlock) {
        self.labels.insert(bb, self.next_instr_addr);
    }

    fn label_for(&self, bb: BasicBlock) -> Option<i16> {
        self.labels.get(&bb).copied()
    }

    fn branch_offset(&self, src_addr: i16, dest: BasicBlock) -> Option<i16> {
        self.label_for(dest).map(|l| l - src_addr)
    }

    fn loc_for_val(&self, val: Value) -> Location<Register> {
        self.loc_table.get(val).expect("invariant violated: missing location for value")
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

    fn visit_bind_instr(&mut self, _val: Value, _loc: CCLocation) {
        todo!();
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
        todo!();
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

    fn visit_move_instr(&mut self, _val: Value, _loc: CCLocation) {
        todo!();
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
