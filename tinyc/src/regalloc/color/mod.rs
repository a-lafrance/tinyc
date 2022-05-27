mod graph;

use std::collections::HashMap;
use crate::ir::isa::{Body, Value};
use self::graph::InterferenceGraph;
use super::{Allocator, Location, LocationTable, RegisterSet};

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

fn assign_colors_to_locations<Reg: RegisterSet>(
    table: &mut LocationTable<Reg>,
    colors: HashMap<Value, usize>,
) {
    let mut color_to_location = HashMap::new();
    let mut next_stack_local = 0;

    for (val, color) in colors.into_iter() {
        let loc = color_to_location.entry(color).or_insert_with(|| {
            // try make register
            // otherwise choose next stack local
            Reg::from_index(color as u8)
                .map(|r| Location::Reg(r))
                .unwrap_or_else(|| {
                    let local = Location::Stack(next_stack_local);
                    next_stack_local += 1;

                    local
                })
        });

        table.insert(val, *loc);
    }
}
