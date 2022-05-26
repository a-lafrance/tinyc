mod graph;

use std::collections::HashMap;
use crate::ir::isa::{Body, Value};
use self::graph::InterferenceGraph;
use super::{Allocator, LocationTable, RegisterSet};

pub struct ColoringAllocator;

impl<R: RegisterSet> Allocator<R> for ColoringAllocator {
    fn build_table(table: &mut LocationTable<R>, body: &Body) {
        // build & color interference graph
        let mut ig = InterferenceGraph::from(body);
        let colors = ig.color();

        // assign colors to actual locations in table
        assign_colors_to_locations(table, colors);
    }
}

fn assign_colors_to_locations(
    _table: &mut LocationTable<impl RegisterSet>,
    _colors: HashMap<Value, usize>,
) {
    todo!();
}
