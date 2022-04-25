use std::collections::{HashMap, HashSet};
use crate::ir::isa::{BasicBlockData, Body, Value};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InterferenceGraph(HashMap<Value, HashSet<Value>>);

impl InterferenceGraph {
    fn visit_basic_block(&mut self, bb: &BasicBlockData, mut live_set: HashSet<Value>) {
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
}

impl From<&Body> for InterferenceGraph {
    fn from(body: &Body) -> InterferenceGraph {
        let mut ig = InterferenceGraph::default();
        let live_set = HashSet::new();

        if let Some(root) = body.root_block_data() {
            ig.visit_basic_block(root, live_set);
        }

        ig
    }
}


#[cfg(test)]
mod tests {
    use maplit::{hashmap, hashset};
    use crate::ir::isa::{BasicBlock, Instruction, StoredBinaryOpcode};
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
