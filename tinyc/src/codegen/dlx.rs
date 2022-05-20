use std::{
    collections::{HashMap},
    io::{BufWriter, Write},
};
use crate::{
    driver::opt::OptConfig,
    ir::{
        isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, CCLocation, ControlFlowEdge, StoredBinaryOpcode, Value},
        visit::{self, IrVisitor},
        IrStore,
    },
};
use dlx::isa::{F1Opcode, F2Opcode, Instruction, Register};

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
    // R20 - R25 are reserved for parameter passing -- this means the first 6 params can be passed in registers
    // R26 - R27 are (naively) reserved as "load/store temporaries" for transferring values to/from the stack
    // any more parameters must be passed on the stack in reverse order
    // the stack frame for a function should look like:
        /*
            -------- <- each line (with content) is a word
            params (as required)
            ...
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
        // prologue: addi RSP, N <- save space for locals
        // ... refer to locals on stack as required
        // ... refer to params on stack as required
        // epilogue: subi RSP, N <- shrink stack back down
// calling convention impl plan:
    // impl ir pass that scans function body and determines its prologue and epilogue
        // pass must determine how much stack space must be allocated, assign param values to their registers/stack vars, etc
    // need a simple regalloc table that just links value to its register/memory cell
        // idea: maintain 2 tables
            // first is the actual register assignment table: for each value in the program, which location is it assigned to
                // this is used for knowing which register refers to a value during its lifetime
            // second is the "register live set": at the current point in the program, which registers are in use and what value is in there
                // this is useful for knowing which registers must be saved across callpoints
            // "real" register allocation's only real impact is to populate the register assignment table
                // this is great because it means that all the infra required to handle locations (stack/register allocations) is already done
                // so when the time comes to integrate real register allocation, it's just algorithm plug-and-play
        // idea: implement trivial register allocation independent of codegen directly
            // meaning, write a trivial register allocator in the regalloc module that just assigns registers sequentially
            // good register allocation should just mean swapping out the allocation algorithm in that case
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
    // new register allocation system (trivial allocation algorithm for now)
    // switch mu instruction to bind/move instructions
    // codegen logic for move/call instructions
    // extend the codegen system to multiple bodies
        // including a way to store the address of each function


struct DlxCodegen<'b> {
    body: &'b Body,
    buffer: Vec<Instruction>,
    labels: HashMap<BasicBlock, i16>, // labels bb number to addr of first instruction
    next_instr_addr: i16, // addr in words of next instr
    unresolved_branches: Vec<UnresolvedBranch>,
    cutoff_point: Option<BasicBlock>,
    known_consts: HashMap<Value, u32>,
    opt: OptConfig,
}

impl<'b> DlxCodegen<'b> {
    pub fn new(body: &'b Body, opt: OptConfig) -> DlxCodegen<'b> {
        DlxCodegen {
            body,
            buffer: Vec::new(),
            labels: HashMap::new(),
            next_instr_addr: 0,
            unresolved_branches: Vec::new(),
            cutoff_point: None,
            known_consts: HashMap::new(),
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
}

impl IrVisitor for DlxCodegen<'_> {
    fn visit_body(&mut self, body: &Body) {
        if let Some((root, root_bb)) = body.root_block_entry() {
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

    fn visit_bind_instr(&mut self, _val: Value, _loc: CCLocation) {
        todo!();
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

    fn visit_move_instr(&mut self, _val: Value, _loc: CCLocation) {
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
