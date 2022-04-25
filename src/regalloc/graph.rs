use std::collections::{HashMap, HashSet};
use crate::ir::isa::{BasicBlockData, Body, Value};

#[derive(Clone, Debug, Default)]
pub struct InterferenceGraph(HashMap<Value, HashSet<Value>>);

impl InterferenceGraph {
    fn visit_basic_block(&mut self, bb: &BasicBlockData, mut live_set: HashSet<Value>) {
        // for each instr from last to first
        for instr in bb.body().iter().rev() {
            // remove dest value from live set if exists
            if let Some(result_val) = instr.result_val() {
                let edges = self.0.entry(result_val).or_default();
                live_set.remove(&result_val);

                // add interferences to graph
                for val in live_set.iter().copied() {
                    // all operands in live set interfere with dest value
                    edges.insert(val);
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
