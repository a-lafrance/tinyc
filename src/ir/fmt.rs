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

pub trait IrFormat: IrVisitor {
    fn fmt(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> io::Result<()>;
}

pub struct IrFormatter<Formatter: IrFormat> {
    fmt: Formatter,
    visited: HashSet<BasicBlock>,
}

impl<Formatter: IrFormat> IrFormatter<Formatter> {
    fn new(fmt: Formatter) -> IrFormatter<Formatter> {
        IrFormatter {
            fmt,
            visited: HashSet::new(),
        }
    }

    pub fn fmt(fmt: Formatter, ir: &IrStore) -> io::Result<()> {
        match ir.root_block() {
            Some(root) => IrFormatter::new(fmt).fmt_basic_block(root, ir),
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
        self.fmt.fmt(bb, bb_data).map(|_| bb_data)
    }
}


pub struct TextFormat<W: Write> {
    writer: W,
    result: io::Result<()>,
}

impl<W: Write> TextFormat<W> {
    pub fn new(writer: W) -> Self {
        Self { writer, result: Ok(()) }
    }

    fn take_result(&mut self) -> io::Result<()> {
        mem::replace(&mut self.result, Ok(()))
    }
}

impl<W: Write> IrFormat for TextFormat<W> {
    fn fmt(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> io::Result<()> {
        self.visit_basic_block(bb, bb_data);
        self.take_result()
    }
}

impl<W: Write> IrVisitor for TextFormat<W> {
    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        self.result = writeln!(self.writer, "{}:", bb);
        visit::walk_basic_block(self, bb_data);
        writeln!(self.writer).ok();
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        self.result = writeln!(self.writer, "  {}", instr);
    }
}
