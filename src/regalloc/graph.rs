use std::collections::{HashMap, HashSet};
use crate::ir::isa::{BasicBlock, BasicBlockData, Body, ControlFlowKind, Value};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InterferenceGraph(HashMap<Value, HashSet<Value>>);

impl InterferenceGraph {
    fn construct_from_basic_block(&mut self, bb: &BasicBlockData, live_set: &mut HashSet<Value>) {
        // for each instr from last to first
        for instr in bb.body().iter().rev() {
            // remove dest value from live set if exists
            if let Some(result_val) = instr.result_val() {
                live_set.remove(&result_val);
                self.0.entry(result_val).or_insert_with(HashSet::new);

                // add interferences to graph
                for val in live_set.iter().copied() {
                    // all operands in live set interfere with dest value
                    let result_edges = self.0.get_mut(&result_val).unwrap();
                    result_edges.insert(val);

                    let val_edges = self.0.entry(val).or_default();
                    val_edges.insert(result_val);
                }
            }

            // insert operands to live set
            for val in instr.operands() {
                live_set.insert(val);
            }
        }
    }

    fn visit_basic_block(&mut self, body: &Body, bb: BasicBlock, live_set: &mut HashSet<Value>, target: Option<BasicBlock>) {
        if target.map(|t| t == bb).unwrap_or(false) {
            return;
        }

        let bb_data = body.basic_block_data(bb);

        match bb_data.control_flow_kind() {
            ControlFlowKind::Leaf => self.construct_from_basic_block(bb_data, live_set),
            ControlFlowKind::FallthroughOnly(dest) | ControlFlowKind::UnconditionalBranch(dest) => {
                self.visit_basic_block(body, dest, live_set, target);
                self.construct_from_basic_block(bb_data, live_set);
            },
            ControlFlowKind::IfStmt(then_bb, else_bb) => self.visit_if_stmt(body, bb_data, then_bb, else_bb, live_set, target),
            // ControlFlowKind::Loop(header, body, follow) => {},
        }
    }

    fn visit_if_stmt(
        &mut self,
        body: &Body,
        condition_bb_data: &BasicBlockData,
        then_bb: BasicBlock,
        else_bb: BasicBlock,
        live_set: &mut HashSet<Value>,
        target: Option<BasicBlock>,
    ) {
        let then_bb_data = body.basic_block_data(then_bb);
        let join_bb = then_bb_data.branch_dest().unwrap(); // this assumes if stmt configured correctly
        self.visit_basic_block(body, join_bb, live_set, target);

        let mut then_live_set = live_set.clone();
        let else_live_set = live_set;

        for (then_val, else_val) in body.basic_block_data(join_bb).phis() {
            then_live_set.remove(&else_val);
            else_live_set.remove(&then_val);
        }

        self.visit_basic_block(body, then_bb, &mut then_live_set, Some(join_bb));
        self.visit_basic_block(body, else_bb, else_live_set, Some(join_bb));

        let live_set = else_live_set;

        for val in then_live_set.into_iter() {
            live_set.insert(val);
        }

        self.construct_from_basic_block(condition_bb_data, live_set);
    }
}

impl From<&Body> for InterferenceGraph {
    fn from(body: &Body) -> InterferenceGraph {
        let mut ig = InterferenceGraph::default();
        let mut live_set = HashSet::new();

        if let Some(root) = body.root_block() {
            // ACTUALLY, don't need to make cfg bidirectional
            // instead, need to make this routine recursive
            // starting at root block, just visit its children first
            // then, visit the root
            // let's concoct an algorithm for this shall we

            // situations:
                // fallthrough ONLY:
                    // visit fallthrough dest
                    // use resulting live set
                    // visit current block
                // no children:
                    // use empty live set
                    // visit current block
                // if statement:
                    // visit then/else together
                    // visit condition block
                // then/else:
                    // visit join block
                    // split up live set
                        // make 2 copies of live set
                        // for each phi in join block:
                            // exclude else operand in then side copy
                            // exclude then operand in else side copy
                    // visit then/else blocks separately with corresponding live set
                        // IMPORTANT: stop visiting when you get to the join block
                    // then union live sets together into master copy



            ig.visit_basic_block(body, root, &mut live_set, None);
        }

        ig
    }
}


#[cfg(test)]
mod tests {
    use maplit::{hashmap, hashset};
    use crate::ir::isa::{Instruction, StoredBinaryOpcode};
    use super::*;

    #[test]
    fn interference_graph_single_bb_sanity_check() {
        /*
            $0 = read
            $1 = read
            write $0

            $2 = add $0, $1
            write $2
        */

        let body = Body::from(
            vec![BasicBlockData::with(
                vec![
                    Instruction::Read(Value(0)),
                    Instruction::Read(Value(1)),
                    Instruction::Write(Value(0)),
                    Instruction::StoredBinaryOp {
                        opcode: StoredBinaryOpcode::Add,
                        src1: Value(0),
                        src2: Value(1),
                        dest: Value(2),
                    },
                    Instruction::Write(Value(2)),
                ],
                None,
                None,
                None,
            )],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph(hashmap! {
            Value(0) => hashset! { Value(1) },
            Value(1) => hashset! { Value(0) },
            Value(2) => HashSet::new(),
        }));
    }
}
