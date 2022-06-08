use super::isa::{BasicBlock, BasicBlockData, Body, BranchOpcode, ControlFlowEdge, CCLocation, Instruction, StoredBinaryOpcode, Value};

pub trait IrVisitor: Sized {
    fn visit_body(&mut self, _body: &Body) { }
    fn visit_basic_block(&mut self, _bb: BasicBlock, bb_data: &BasicBlockData) {
        walk_basic_block(self, bb_data);
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        walk_instr(self, instr);
    }

    fn visit_bind_instr(&mut self, _val: Value, _loc: CCLocation) { }
    fn visit_branch_instr(&mut self, _opcode: BranchOpcode, _cmp: Value, _dest: BasicBlock) { }
    fn visit_call_instr(&mut self, _func: &str) { }
    fn visit_const_instr(&mut self, _const_val: i32, _dest: Value) { }
    fn visit_end_instr(&mut self) { }
    fn visit_move_instr(&mut self, _val: Value, _loc: CCLocation) { }
    fn visit_nop_instr(&mut self) { }
    fn visit_read_instr(&mut self, _dest: Value) { }
    fn visit_return_instr(&mut self) { }
    fn visit_stored_binop_instr(&mut self, _opcode: StoredBinaryOpcode, _src1: Value, _src2: Value, _dest: Value) { }
    fn visit_unconditional_branch_instr(&mut self, _dest: BasicBlock) { }
    fn visit_write_instr(&mut self, _src: Value) { }
    fn visit_writeln_instr(&mut self) { }
}

pub fn walk_body(v: &mut impl IrVisitor, body: &Body) {
    if let Some((root, root_data)) = body.root_block_entry() {
        walk_basic_block_in_body(v, root, root_data, body, None);
    }
}

fn walk_basic_block_in_body(
    v: &mut impl IrVisitor,
    bb: BasicBlock,
    bb_data: &BasicBlockData,
    body: &Body,
    cutoff: Option<BasicBlock>
) {
    if Some(bb) != cutoff {
        v.visit_basic_block(bb, bb_data);

        match bb_data.edge() {
            ControlFlowEdge::Leaf => (),
            ControlFlowEdge::Fallthrough(dest) | ControlFlowEdge::Branch(dest) => {
                let dest_data = body.basic_block_data(dest);
                walk_basic_block_in_body(v, dest, dest_data, body, cutoff);
            },
            ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb) => {
                let then_bb_data = body.basic_block_data(then_bb);

                match else_bb {
                    Some(else_bb) => {
                        let else_bb_data = body.basic_block_data(else_bb);

                        walk_basic_block_in_body(v, then_bb, then_bb_data, body, Some(join_bb));
                        walk_basic_block_in_body(v, else_bb, else_bb_data, body, cutoff);
                    },

                    None => {
                        walk_basic_block_in_body(v, then_bb, then_bb_data, body, cutoff);
                    },
                }
            },
            ControlFlowEdge::Loop(body_bb, follow_bb) => {
                let body_bb_data = body.basic_block_data(body_bb);
                walk_basic_block_in_body(v, body_bb, body_bb_data, body, Some(bb));

                let follow_bb_data = body.basic_block_data(follow_bb);
                walk_basic_block_in_body(v, follow_bb, follow_bb_data, body, cutoff);
            }
        }
    }
}

pub fn walk_basic_block(v: &mut impl IrVisitor, bb: &BasicBlockData) {
    for instr in bb.body().iter() {
        v.visit_instr(instr);
    }
}

pub fn walk_instr(v: &mut impl IrVisitor, instr: &Instruction) {
    match instr {
        Instruction::Bind(val, loc) => v.visit_bind_instr(*val, *loc),
        Instruction::Branch(opcode, cmp, dest) => v.visit_branch_instr(*opcode, *cmp, *dest),
        Instruction::Call(func) => v.visit_call_instr(func),
        Instruction::Const(const_val, dest) => v.visit_const_instr(*const_val, *dest),
        Instruction::End => v.visit_end_instr(),
        Instruction::Move(val, loc) => v.visit_move_instr(*val, *loc),
        Instruction::Nop => v.visit_nop_instr(),
        Instruction::Read(dest) => v.visit_read_instr(*dest),
        Instruction::Return => v.visit_return_instr(),
        Instruction::StoredBinaryOp(opcode, src1, src2, dest) => v.visit_stored_binop_instr(*opcode, *src1, *src2, *dest),
        Instruction::Write(src) => v.visit_write_instr(*src),
        Instruction::Writeln => v.visit_writeln_instr(),
        Instruction::UnconditionalBranch(dest) => v.visit_unconditional_branch_instr(*dest),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct VisitChecker {
        tape: String,
    }

    impl VisitChecker {
        pub fn new() -> VisitChecker {
            VisitChecker { tape: String::new() }
        }

        pub fn push(&mut self, marker: char) {
            self.tape.push(marker);
        }

        pub fn push_str(&mut self, marker: &str) {
            self.tape.push_str(marker);
        }

        pub fn tape(&self) -> &str {
            &self.tape
        }
    }

    impl IrVisitor for VisitChecker {
        fn visit_body(&mut self, body: &Body) {
            self.push('b');
            walk_body(self, body);
        }

        fn visit_basic_block(&mut self, _: BasicBlock, bb_data: &BasicBlockData) {
            self.push_str("bk");
            walk_basic_block(self, bb_data);
        }

        fn visit_instr(&mut self, instr: &Instruction) {
            self.push('i');
            walk_instr(self, instr);
        }

        fn visit_bind_instr(&mut self, _: Value, _: CCLocation) {
            self.push_str("bn");
        }

        fn visit_branch_instr(&mut self, opcode: BranchOpcode, _: Value, _: BasicBlock) {
            match opcode {
                BranchOpcode::Beq => self.push_str("=="),
                BranchOpcode::Bne => self.push_str("!="),
                BranchOpcode::Bgt => self.push_str(">"),
                BranchOpcode::Bge => self.push_str(">="),
                BranchOpcode::Blt => self.push_str("<"),
                BranchOpcode::Ble => self.push_str("<="),
            }
        }

        fn visit_call_instr(&mut self, _: &str) {
            self.push('c');
        }

        fn visit_const_instr(&mut self, _: i32, _: Value) {
            self.push_str("cv");
        }

        fn visit_end_instr(&mut self) {
            self.push('e');
        }

        fn visit_move_instr(&mut self, _: Value, _: CCLocation) {
            self.push_str("mv");
        }

        fn visit_nop_instr(&mut self) {
            self.push('n');
        }

        fn visit_read_instr(&mut self, _: Value) {
            self.push('r');
        }

        fn visit_return_instr(&mut self) {
            self.push_str("rt");
        }

        fn visit_stored_binop_instr(&mut self, opcode: StoredBinaryOpcode, _: Value, _: Value, _: Value) {
            match opcode {
                StoredBinaryOpcode::Add => self.push('a'),
                StoredBinaryOpcode::Sub => self.push('s'),
                StoredBinaryOpcode::Mul => self.push('m'),
                StoredBinaryOpcode::Div => self.push('d'),
                StoredBinaryOpcode::Cmp => self.push_str("co" /* -mpare */),
                StoredBinaryOpcode::Phi => self.push('p'),
            }
        }

        fn visit_unconditional_branch_instr(&mut self, _: BasicBlock) {
            self.push('u');
        }

        fn visit_write_instr(&mut self, _src: Value) {
            self.push('w');
        }

        fn visit_writeln_instr(&mut self) {
            self.push_str("wl");
        }
    }

    #[test]
    fn walk_body_sanity_check() {
        /*
            BB0:
                $0 = read
                $1 = read
                $2 = cmp $0, $1
                ble $2, BB2

            BB1:
                write $0
                br BB3

            BB2:
                write $1

            BB3:
                writeln
                end
        */

        let mut v = VisitChecker::new();
        v.visit_body(&Body::from(
            vec![
                BasicBlockData::with(
                    vec![
                        Instruction::Read(Value(0)),
                        Instruction::Read(Value(1)),
                        Instruction::StoredBinaryOp(StoredBinaryOpcode::Cmp, Value(0), Value(1), Value(2)),
                        Instruction::Branch(BranchOpcode::Ble, Value(2), BasicBlock(2)),
                    ],
                    ControlFlowEdge::IfStmt(BasicBlock(1), Some(BasicBlock(2)), BasicBlock(3)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(0)),
                        Instruction::UnconditionalBranch(BasicBlock(3)),
                    ],
                    ControlFlowEdge::Branch(BasicBlock(3)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(1)),
                    ],
                    ControlFlowEdge::Fallthrough(BasicBlock(3)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Writeln,
                        Instruction::End,
                    ],
                    ControlFlowEdge::Leaf,
                    None,
                    HashMap::new(),
                ),
            ],
            Some(BasicBlock(0)),
        ));

        assert_eq!(v.tape(), "bbkiriricoi<=bkiwiubkiwbkiwlie");
    }

    #[test]
    fn walk_basic_block_sanity_check() {
        let mut v = VisitChecker::new();
        v.visit_basic_block(BasicBlock(0), &BasicBlockData::with(
            vec![
                Instruction::Read(Value(0)),
                Instruction::Write(Value(0)),
                Instruction::Writeln,
                Instruction::End,
            ],
            ControlFlowEdge::Leaf,
            None,
            HashMap::new(),
        ));

        assert_eq!(v.tape(), "bkiriwiwlie");
    }

    #[test]
    fn walk_instr_sanity_check() {
        let mut v = VisitChecker::new();
        let instrs = [
            Instruction::Bind(Value(0), CCLocation::RetVal),
            Instruction::Branch(BranchOpcode::Beq, Value(0), BasicBlock(0)),
            Instruction::Call("function".to_string()),
            Instruction::Const(0, Value(0)),
            Instruction::End,
            Instruction::Move(Value(0), CCLocation::RetVal),
            Instruction::Nop,
            Instruction::Read(Value(0)),
            Instruction::Return,
            Instruction::StoredBinaryOp(StoredBinaryOpcode::Phi, Value(0), Value(0), Value(0)),
            Instruction::UnconditionalBranch(BasicBlock(0)),
            Instruction::Write(Value(0)),
            Instruction::Writeln,
        ];

        for i in instrs.into_iter() {
            v.visit_instr(&i);
        }

        assert_eq!(v.tape(), "ibni==icicvieimvinirirtipiuiwiwl");
    }
}
