use std::{
    collections::HashSet,
    io::{self, Write},
    mem,
};
use super::{
    isa::{BasicBlock, BasicBlockData, Instruction},
    visit::{self, IrVisitor},
    IrStore,
};

pub struct IrFormatter<W: Write> {
    fmt: IrFormatVisitor<W>,
    visited: HashSet<BasicBlock>,
}

impl<W: Write> IrFormatter<W> {
    fn new(writer: W) -> IrFormatter<W> {
        IrFormatter {
            fmt: IrFormatVisitor::new(writer),
            visited: HashSet::new(),
        }
    }

    pub fn fmt(writer: W, ir: &IrStore) -> io::Result<()> {
        match ir.root_block() {
            Some(root) => IrFormatter::new(writer).fmt_basic_block(root, ir),
            None => Ok(()),
        }
    }

    fn fmt_basic_block(&mut self, bb: BasicBlock, store: &IrStore) -> io::Result<()> {
        if !self.visited.contains(&bb) {
            self.visited.insert(bb);
            let bb_data = self.fmt_single_basic_block(bb, store)?;

            if let Some(fallthrough_bb) = bb_data.fallthrough_dest() {
                self.fmt_basic_block(fallthrough_bb, store)?;
            }

            if let Some(branch_bb) = bb_data.branch_dest() {
                self.fmt_basic_block(branch_bb, store)?;
            }
        }

        Ok(())
    }

    /// This function only exists to make error handling nicer
    fn fmt_single_basic_block<'ir>(&mut self, bb: BasicBlock, store: &'ir IrStore) -> io::Result<&'ir BasicBlockData> {
        let bb_data = store.basic_block_data(bb);
        self.fmt.visit_basic_block(bb, bb_data);
        self.fmt.take_result().map(|_| bb_data)
    }
}

struct IrFormatVisitor<W: Write> {
    writer: W,
    result: io::Result<()>,
}

impl<W: Write> IrFormatVisitor<W> {
    fn new(writer: W) -> Self {
        Self { writer, result: Ok(()) }
    }

    fn take_result(&mut self) -> io::Result<()> {
        mem::replace(&mut self.result, Ok(()))
    }
}

impl<W: Write> IrVisitor for IrFormatVisitor<W> {
    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        self.result = writeln!(self.writer, "{}:", bb);
        visit::walk_basic_block(self, bb_data);
        writeln!(self.writer).ok();
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        self.result = writeln!(self.writer, "  {}", instr);
    }
}
