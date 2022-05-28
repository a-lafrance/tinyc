use std::collections::{BTreeSet, HashMap, HashSet};
use crate::ir::isa::{BasicBlock, BasicBlockData, Body, ControlFlowEdge, Value};

#[derive(Clone, Debug, Default, PartialEq)]
struct NodeEntry {
    edges: HashSet<Value>,
    cost: usize,
}

impl NodeEntry {
    pub fn has_edge(&self, dest: Value) -> bool {
        self.edges.contains(&dest)
    }

    pub fn add_edge(&mut self, dest: Value) {
        self.edges.insert(dest);
    }

    pub fn remove_edge(&mut self, dest: Value) {
        self.edges.remove(&dest);
    }

    pub fn edges(&self) -> impl Iterator<Item = Value> + '_ {
        self.edges.iter().copied()
    }

    pub fn into_edges(self) -> impl IntoIterator<Item = Value> {
        self.edges.into_iter()
    }

    pub fn cost(&self) -> usize {
        self.cost
    }

    pub fn increase_cost(&mut self) {
        self.cost += 1;
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InterferenceGraph {
    // implement clusters by repurposing one of their nodes as the "root" node.
    // just remove the other nodes and edges from the graph and instead add them to the cluster.
    // then store a directory of clusters to know which nodes are and aren't supernodes for a cluster.
    clusters: HashMap<Value, HashSet<Value>>,
    nodes: HashMap<Value, NodeEntry>,
}

impl InterferenceGraph {
    pub fn color(&mut self) -> HashMap<Value, usize> {
        let mut mapping = HashMap::new();
        self.color_impl(&mut mapping, &mut BTreeSet::new());

        // resolve clusters by giving each node the same color as the root
        for (root, nodes) in self.clusters.iter() {
            let cluster_color = mapping.get(root).copied().unwrap(); // FIXME: audit this

            for node in nodes.iter().copied() {
                mapping.insert(node, cluster_color);
            }
        }

        mapping
    }

    fn color_impl(&mut self, mapping: &mut HashMap<Value, usize>, colors: &mut BTreeSet<usize>) {
        if !self.is_empty() {
            // remove lowest cost node and its edges
                // SAVE THEM SOMEWHERE
                // DON'T FORGET TO REMOVE EDGES FROM THE OTHER NODES' EDGE POOLS TOO
            let (node, entry) = self.remove_lowest_cost_node().unwrap(); // FIXME: unsafe

            // recursively color (call self.color recursively)
            self.color_impl(mapping, colors);

            // add node and edges back to graph
            // choose color for node that's different from its neighbors
                // always prefer colors that have already been assigned
                // if no color exists, "allocate" a new one
            self.reinstate_node(node, entry);

            // to pick a color for the node:
                // take the diff between all currently assigned colors and the colors present among neighbors
                // if a value exists, choose that one
                // otherwise, add a new assigned color and use that one
            // once you've chosen a color, stick it in the mapping
            let color = self.pick_color(node, mapping, colors);
            mapping.insert(node, color);

            // important wrinkle: take mu instructions into account such that you prefer to stick those values
            // in the right register out of the gate
                // this is a wrinkle to add later lol
        }
    }

    fn pick_color(&self, node: Value, mapping: &HashMap<Value, usize>, colors: &mut BTreeSet<usize>) -> usize {
        // May be able to speed this up by detecting color stuff based on max color and number of neighbors? idk
        let entry = self.nodes.get(&node).unwrap();
        let neighbor_colors: BTreeSet<_> = entry.edges()
            .map(|n| mapping.get(&n).copied().unwrap()) // FIXME: unsafe
            .collect();

        colors.difference(&neighbor_colors).copied()
            .min()
            .unwrap_or_else(|| {
                let next_color = colors.iter().copied().max().map(|c| c + 1).unwrap_or(0); // FIXME: a bit unsafe
                colors.insert(next_color);

                next_color
            })
    }

    fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    fn has_edge(&self, v1: Value, v2: Value) -> bool {
        self.nodes.get(&v1).map(|e| e.has_edge(v2)).unwrap_or(false)
    }

    fn add_edge(&mut self, v1: Value, v2: Value) {
        let v1_entry = self.nodes.entry(v1).or_default();
        v1_entry.add_edge(v2);

        let v2_entry = self.nodes.entry(v2).or_default();
        v2_entry.add_edge(v1);
    }

    fn reinstate_node(&mut self, node: Value, entry: NodeEntry) {
        // Reinstate the entry if it was missing in the first place
        let edges: Vec<_> = self.nodes.entry(node).or_insert(entry).edges().collect();

        for dest in edges.into_iter() {
            self.add_edge(node, dest);
        }
    }

    fn remove_edge(&mut self, v1: Value, v2: Value) {
        if let Some(entry) = self.nodes.get_mut(&v1) {
            entry.remove_edge(v2);
        }

        if let Some(entry) = self.nodes.get_mut(&v2) {
            entry.remove_edge(v1);
        }
    }

    fn remove_node(&mut self, v: Value) -> Option<NodeEntry> {
        let entry = self.nodes.remove(&v)?;

        for dest in entry.edges() {
            self.remove_edge(v, dest);
        }

        Some(entry)
    }

    fn remove_lowest_cost_node(&mut self) -> Option<(Value, NodeEntry)> {
        let node = self.nodes.iter()
            .map(|(n, e)| (e.cost(), *n))
            .min()
            .map(|(_, n)| n)?;

        self.remove_node(node).map(|e| (node, e))
    }

    fn coalesce_live_ranges(&mut self, body: &Body) {
        for bb in body.blocks() {
            for (lhs, rhs, result) in bb.phis() {
                // TODO: track cluster membership and substitute lhs/rhs/dest val for its cluster root
                // if it's involved in another phi
                // Also need to merge clusters if that happens
                let mut cluster = HashSet::with_capacity(3);

                if !self.has_edge(lhs, rhs) {
                    if !self.has_edge(lhs, result) {
                        cluster.insert(lhs);
                        self.merge_nodes(lhs, result);
                    }

                    if !self.has_edge(rhs, result) {
                        cluster.insert(rhs);
                        self.merge_nodes(rhs, result);
                    }
                }

                if !cluster.is_empty() {
                    cluster.insert(result);
                    self.clusters.insert(result, cluster);
                }
            }
        }
    }

    fn merge_nodes(&mut self, src: Value, dest: Value) {
        if let Some(entry) = self.nodes.remove(&src) {
            let dest_entry = self.nodes.get_mut(&dest).unwrap();
            dest_entry.cost += entry.cost;

            for node in entry.into_edges() {
                self.remove_edge(src, node);
                self.add_edge(dest, node);
            }
        }
    }

    fn construct_from_basic_block(&mut self, bb: &BasicBlockData, live_set: &mut HashSet<Value>, track_costs: bool) {
        // for each instr from last to first
        for instr in bb.body().iter().rev() {
            // remove dest value from live set if exists
            if let Some(result_val) = instr.result_val() {
                if live_set.remove(&result_val) {
                    self.nodes.entry(result_val).or_default();

                    // add interferences to graph
                    for val in live_set.iter().copied() {
                        // all operands in live set interfere with dest value
                        self.add_edge(val, result_val);
                    }
                }
            }

            // insert operands to live set
            for val in instr.operands() {
                if track_costs {
                    let entry = self.nodes.entry(val).or_default();
                    entry.increase_cost();
                }

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
            ControlFlowEdge::Leaf => self.construct_from_basic_block(bb_data, live_set, true),
            ControlFlowEdge::Fallthrough(dest) | ControlFlowEdge::Branch(dest) => {
                self.visit_basic_block(body, dest, live_set, target);
                self.construct_from_basic_block(bb_data, live_set, true);
            },
            ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb) => self.visit_if_stmt(body, bb_data, then_bb, else_bb, join_bb, live_set, target),
            ControlFlowEdge::Loop(body_bb, follow_bb) => self.visit_loop(body, bb, body_bb, follow_bb, live_set, target),
        }
    }

    // it's pretty benign if there's lots of arguments here because it's pretty obvious what they mean
    #[allow(clippy::too_many_arguments)]
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

        self.construct_from_basic_block(condition_bb_data, live_set, true);
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
        self.construct_from_basic_block(header_bb_data, live_set, true);

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
        self.construct_from_basic_block(header_bb_data, live_set, false);

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
                HashMap::new(),
            )],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            nodes: hashmap!{
                Value(0) => NodeEntry {
                    edges: hashset!{ Value(1) },
                    cost: 2,
                },
                Value(1) => NodeEntry {
                    edges: hashset!{ Value(0) },
                    cost: 1,
                },
                Value(2) => NodeEntry {
                    edges: HashSet::new(),
                    cost: 1,
                },
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
                HashMap::new(),
            )],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            nodes: hashmap!{
                Value(0) => NodeEntry {
                    edges: hashset!{ Value(1) },
                    cost: 1,
                },
                Value(1) => NodeEntry {
                    edges: hashset!{ Value(0) },
                    cost: 1,
                },
                Value(2) => NodeEntry {
                    edges: HashSet::new(),
                    cost: 1,
                },
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
            $2 = cmp $0, $1
            ble $2, BB2

            BB1:
            write $0
            br BB3

            BB2:
            write $1

            BB3:
            $3 = add $0, $1
            write $3
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
                    HashMap::new(),
                ),
            ],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap!{},
            nodes: hashmap! {
                Value(0) => NodeEntry {
                    edges: hashset! { Value(1), Value(2) },
                    cost: 3
                },
                Value(1) => NodeEntry {
                    edges: hashset! { Value(0), Value(2) },
                    cost: 3,
                },
                Value(2) => NodeEntry {
                    edges: hashset! { Value(0), Value(1) },
                    cost: 1,
                },
                Value(3) => NodeEntry {
                    edges: HashSet::new(),
                    cost: 1,
                },
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
                    HashMap::new(),
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
                    HashMap::new(),
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
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Write(Value(4)),
                        Instruction::Writeln,
                        Instruction::End,
                    ],
                    ControlFlowEdge::Leaf,
                    None,
                    HashMap::new(),
                ),
            ],
            Some(BasicBlock(0)),
        );

        let ig = InterferenceGraph::from(&body);
        assert_eq!(ig, InterferenceGraph {
            clusters: hashmap! {
                Value(4) => hashset!{Value(4), Value(0), Value(3)}
            },
            nodes: hashmap! {
                Value(1) => NodeEntry {
                    edges: hashset! { Value(2), Value(4), Value(5) },
                    cost: 1,
                },
                Value(2) => NodeEntry {
                    edges: hashset! { Value(1), Value(4), Value(5) },
                    cost: 1,
                },
                Value(4) => NodeEntry {
                    edges: hashset! { Value(1), Value(2), Value(5) },
                    cost: 5,
                },
                Value(5) => NodeEntry {
                    edges: hashset! { Value(1), Value(2), Value(4) },
                    cost: 1,
                },
            }
        });
    }

    #[test]
    fn ig_coloring_sanity_check() {
        let body = Body::from(
            vec![
                BasicBlockData::with(
                    vec![
                        Instruction::Const(0, Value(1)),
                        Instruction::Const(1, Value(3)),
                        Instruction::Const(2, Value(5)),
                    ],
                    ControlFlowEdge::Fallthrough(BasicBlock(1)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::Read(Value(0)),
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Cmp,
                            Value(0),
                            Value(1),
                            Value(2),
                        ),
                        Instruction::Branch(BranchOpcode::Bne, Value(2), BasicBlock(3)),
                    ],
                    ControlFlowEdge::IfStmt(BasicBlock(2), Some(BasicBlock(3)), BasicBlock(4)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Add,
                            Value(0),
                            Value(3),
                            Value(4),
                        ),
                        Instruction::UnconditionalBranch(BasicBlock(4)),
                    ],
                    ControlFlowEdge::Branch(BasicBlock(4)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Mul,
                            Value(0),
                            Value(5),
                            Value(6),
                        ),
                    ],
                    ControlFlowEdge::Fallthrough(BasicBlock(4)),
                    None,
                    HashMap::new(),
                ),
                BasicBlockData::with(
                    vec![
                        Instruction::StoredBinaryOp(
                            StoredBinaryOpcode::Phi,
                            Value(4),
                            Value(6),
                            Value(7),
                        ),
                        Instruction::Write(Value(7)),
                        Instruction::Writeln,
                        Instruction::End,
                    ],
                    ControlFlowEdge::Leaf,
                    None,
                    HashMap::new(),
                ),
            ],
            Some(BasicBlock(0))
        );

        let mut ig = InterferenceGraph::from(&body);
        let colors = ig.color();

        // just quickly make sure the thing was colored optimally
        assert_eq!(colors.values().copied().max(), Some(3));
    }
}
