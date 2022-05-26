use std::collections::{HashMap, HashSet};
use crate::ir::isa::{BasicBlock, BasicBlockData, Body, ControlFlowEdge, Value};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InterferenceGraph {
    // implement clusters by repurposing one of their nodes as the "root" node.
    // just remove the other nodes and edges from the graph and instead add them to the cluster.
    // then store a directory of clusters to know which nodes are and aren't supernodes for a cluster.
    clusters: HashMap<Value, Vec<Value>>,
    edges: HashMap<Value, HashSet<Value>>,
}

impl InterferenceGraph {
    fn has_edge(&self, v1: Value, v2: Value) -> bool {
        self.edges.get(&v1).map(|e| e.contains(&v2)).unwrap_or(false)
    }

    fn add_edge(&mut self, v1: Value, v2: Value) {
        let v1_edges = self.edges.entry(v1).or_insert_with(HashSet::new);
        v1_edges.insert(v2);

        let v2_edges = self.edges.entry(v2).or_insert_with(HashSet::new);
        v2_edges.insert(v1);
    }

    fn remove_edge(&mut self, v1: Value, v2: Value) {
        if let Some(edges) = self.edges.get_mut(&v1) {
            edges.remove(&v2);
        }

        if let Some(edges) = self.edges.get_mut(&v2) {
            edges.remove(&v1);
        }
    }

    fn coalesce_live_ranges(&mut self, body: &Body) {
        for bb in body.blocks() {
            for (lhs, rhs, result) in bb.phis() {
                let mut cluster = Vec::with_capacity(3);
                cluster.push(result);

                if !self.has_edge(lhs, rhs) {
                    if !self.has_edge(lhs, result) {
                        cluster.push(lhs);

                        if let Some(edges) = self.edges.remove(&lhs) {
                            for node in edges.into_iter() {
                                self.remove_edge(lhs, node);
                                self.add_edge(result, node);
                            }
                        }
                    }

                    if !self.has_edge(rhs, result) {
                        cluster.push(rhs);

                        if let Some(edges) = self.edges.remove(&rhs) {
                            for node in edges.into_iter() {
                                self.remove_edge(rhs, node);
                                self.add_edge(result, node);
                            }
                        }
                    }
                }

                if cluster.len() > 1 {
                    self.clusters.insert(result, cluster);
                }
            }
        }
    }

    fn construct_from_basic_block(&mut self, bb: &BasicBlockData, live_set: &mut HashSet<Value>) {
        // for each instr from last to first
        for instr in bb.body().iter().rev() {
            // remove dest value from live set if exists
            if let Some(result_val) = instr.result_val() {
                live_set.remove(&result_val);
                self.edges.entry(result_val).or_insert_with(HashSet::new);

                // add interferences to graph
                for val in live_set.iter().copied() {
                    // all operands in live set interfere with dest value
                    self.add_edge(val, result_val);
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

        match bb_data.edge() {
            ControlFlowEdge::Leaf => self.construct_from_basic_block(bb_data, live_set),
            ControlFlowEdge::Fallthrough(dest) | ControlFlowEdge::Branch(dest) => {
                self.visit_basic_block(body, dest, live_set, target);
                self.construct_from_basic_block(bb_data, live_set);
            },
            ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb) => self.visit_if_stmt(body, bb_data, then_bb, else_bb, join_bb, live_set, target),
            ControlFlowEdge::Loop(body_bb, follow_bb) => self.visit_loop(body, bb, body_bb, follow_bb, live_set, target),
        }
    }

    fn visit_if_stmt(
        &mut self,
        body: &Body,
        condition_bb_data: &BasicBlockData,
        then_bb: BasicBlock,
        else_bb: Option<BasicBlock>,
        join_bb: BasicBlock,
        live_set: &mut HashSet<Value>,
        target: Option<BasicBlock>,
    ) {
        self.visit_basic_block(body, join_bb, live_set, target);

        let mut then_live_set = live_set.clone();

        for (then_val, alt_val, _) in body.basic_block_data(join_bb).phis() {
            then_live_set.remove(&alt_val);
            live_set.remove(&then_val);
        }

        self.visit_basic_block(body, then_bb, &mut then_live_set, Some(join_bb));

        if let Some(else_bb) = else_bb {
            self.visit_basic_block(body, else_bb, live_set, Some(join_bb));
        }

        for val in then_live_set.into_iter() {
            live_set.insert(val);
        }

        self.construct_from_basic_block(condition_bb_data, live_set);
    }

    fn visit_loop(
        &mut self,
        body: &Body,
        header_bb: BasicBlock,
        body_bb: BasicBlock,
        follow_bb: BasicBlock,
        live_set: &mut HashSet<Value>,
        target: Option<BasicBlock>,
    ) {
        // visit follow bb
        let header_bb_data = body.basic_block_data(header_bb);
        self.visit_basic_block(body, follow_bb, live_set, target);

        // use live set, construct from header bb
        let original_live_set = live_set.clone();
        self.construct_from_basic_block(header_bb_data, live_set);

        // config live set for body
            // remove left side of phis
        let mut body_live_set = live_set.clone();

        for (prev_val, _, _) in header_bb_data.phis() {
            body_live_set.remove(&prev_val);
        }

        // visit body bb (target = header bb)
        self.visit_basic_block(body, body_bb, &mut body_live_set, Some(header_bb));

        // combine body live set with original live set
            // double check this

        let diff: HashSet<_> = live_set.difference(&original_live_set).copied().collect();

        for val in diff.into_iter() {
            live_set.remove(&val);
        }

        for val in body_live_set.into_iter() {
            live_set.insert(val);
        }

        // construct from header bb again
        self.construct_from_basic_block(header_bb_data, live_set);

        // config live set for prev bb
            // remove right side of phis
        for (_, body_val, _) in header_bb_data.phis() {
            live_set.remove(&body_val);
        }
    }
}

impl From<&Body> for InterferenceGraph {
    fn from(body: &Body) -> InterferenceGraph {
        let mut ig = InterferenceGraph::default();
        let mut live_set = HashSet::new();

        if let Some(root) = body.root_block() {
            ig.visit_basic_block(body, root, &mut live_set, None);
        }

        ig.coalesce_live_ranges(body);
        ig
    }
}


#[cfg(test)]
mod tests {
    use maplit::{hashmap, hashset};
    use crate::ir::isa::{BranchOpcode, Instruction, StoredBinaryOpcode};
    use super::*;

    #[test]
    fn construct_ig_from_single_bb() {
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
                    Instruction::StoredBinaryOp(
                        StoredBinaryOpcode::Add,
                        Value(0),
                        Value(1),
                        Value(2),
                    ),
                    Instruction::Write(Value(2)),
                    Instruction::End,
                ],
                ControlFlowEdge::Leaf,
                None,
            )],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            edges: hashmap!{
                Value(0) => hashset!{ Value(1) },
                Value(1) => hashset!{ Value(0) },
                Value(2) => HashSet::new(),
            },
        });
    }

    #[test]
    fn construct_ig_from_fallthrough_only_blocks() {
        /*
            BB0:
            $0 = const 0
            $1 = const 1

            BB1:
            $2 = add $0, $1
            write $2
            end
        */

        // NOTE: uhh this is just a single block?
        let body = Body::from(
            vec![BasicBlockData::with(
                vec![
                    Instruction::Const(0, Value(0)),
                    Instruction::Const(1, Value(1)),
                    Instruction::StoredBinaryOp(
                        StoredBinaryOpcode::Add,
                        Value(0),
                        Value(1),
                        Value(2),
                    ),
                    Instruction::Write(Value(2)),
                ],
                ControlFlowEdge::Leaf,
                None,
            )],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            edges: hashmap!{
                Value(0) => hashset!{ Value(1) },
                Value(1) => hashset!{ Value(0) },
                Value(2) => HashSet::new(),
            },
        });
    }

    #[test]
    fn construct_ig_from_if_stmt() {
        // outputs the max of two input values
        /*
            BB0:
            $0 = read
            $1 = read
            cmp $0, $1
            ble BB2

            BB1:
            write $0
            br BB3

            BB2:
            write $1

            BB3:
            $2 = add $0, $1
            write $2
            writeln
            end
        */

        let body = Body::from(
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
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(0)),
                        Instruction::UnconditionalBranch(BasicBlock(3)),
                    ],
                    ControlFlowEdge::Branch(BasicBlock(3)),
                    None,
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(1)),
                    ],
                    ControlFlowEdge::Fallthrough(BasicBlock(3)),
                    None,
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Add,
                            Value(0),
                            Value(1),
                            Value(3),
                        ),
                        Instruction::Write(Value(3)),
                        Instruction::Writeln,
                        Instruction::End,
                    ],
                    ControlFlowEdge::Leaf,
                    None,
                ),
            ],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            edges: hashmap! {
                Value(0) => hashset! { Value(1), Value(2) },
                Value(1) => hashset! { Value(0), Value(2) },
                Value(2) => hashset! { Value(0), Value(1) },
                Value(3) => HashSet::new(),
            },
        });
    }

    #[test]
    fn construct_ig_from_loop() {
        /*
            BB0:
                $0 = const 0
                $1 = const 1
                $2 = read

            BB1:
                $4 = phi $0, $3
                $5 = cmp $4, $2
                bge $5, BB3

            BB2:
                $3 = add $4, $1
                br BB1

            BB3:
                write $4
                writeln
                end
        */

        let body = Body::from(
            vec![
                BasicBlockData::with(
                    vec![
                        Instruction::Const(0, Value(0)),
                        Instruction::Const(1, Value(1)),
                        Instruction::Read(Value(2)),
                    ],
                    ControlFlowEdge::Fallthrough(BasicBlock(1)),
                    None,
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Phi,
                            Value(0),
                            Value(3),
                            Value(4),
                        ),
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Cmp,
                            Value(4),
                            Value(2),
                            Value(5),
                        ),
                        Instruction::Branch(BranchOpcode::Bge, Value(5), BasicBlock(3)),
                    ],
                    ControlFlowEdge::Loop(BasicBlock(2), BasicBlock(3)),
                    None,
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Add,
                            Value(4),
                            Value(1),
                            Value(3),
                        ),
                        Instruction::UnconditionalBranch(BasicBlock(1)),
                    ],
                    ControlFlowEdge::Branch(BasicBlock(1)),
                    None,
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(4)),
                        Instruction::Writeln,
                        Instruction::End,
                    ],
                    ControlFlowEdge::Leaf,
                    None,
                ),
            ],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap! {
                Value(4) => vec![Value(4), Value(0), Value(3)]
            },
            edges: hashmap! {
                Value(1) => hashset! { Value(2), Value(4), Value(5) },
                Value(2) => hashset! { Value(1), Value(4), Value(5) },
                Value(4) => hashset! { Value(1), Value(2), Value(5) },
                Value(5) => hashset! { Value(1), Value(2), Value(4) },
            }
        });
    }
}
