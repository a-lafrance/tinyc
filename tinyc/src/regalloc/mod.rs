mod graph;

use crate::ir::isa::Body;
use self::graph::InterferenceGraph;

pub fn alloc(body: &Body) /* return TODO */ {
    let ig = InterferenceGraph::from(body);
}
