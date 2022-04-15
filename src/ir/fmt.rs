use std::fmt::{self, Formatter};
use super::{
    isa::{BasicBlock, BasicBlockData, Instruction},
    visit::{self, IrVisitor},
    IrStore,
};

pub struct IrFormatter<'f>(IrFormatVisitor<'f>);

impl<'f> IrFormatter<'f> {
    pub fn new(f: &'f mut Formatter<'f>) -> IrFormatter {
        IrFormatter(IrFormatVisitor::new(f))
    }

    pub fn fmt(&mut self, ir: &IrStore) -> fmt::Result {
        match ir.root_block() {
            Some(root) => self.fmt_basic_block(root, ir),
            None => Ok(()),
        }
    }

    fn fmt_basic_block(&mut self, bb: BasicBlock, store: &IrStore) -> fmt::Result {
        let bb_data = self.fmt_single_basic_block(bb, store)?;

        if let Some(fallthrough_bb) = bb_data.fallthrough_dest() {
            self.fmt_basic_block(fallthrough_bb, store)?;
        }

        if let Some(branch_bb) = bb_data.branch_dest() {
            self.fmt_basic_block(branch_bb, store)?;
        }

        Ok(())
    }

    /// This function only exists to make error handling nicer
    fn fmt_single_basic_block<'ir>(&mut self, bb: BasicBlock, store: &'ir IrStore) -> Result<&'ir BasicBlockData, fmt::Error> {
        let bb_data = store.basic_block_data(bb);
        self.0.visit_basic_block(bb, bb_data);
        self.0.result.map(|_| bb_data)
    }
}

struct IrFormatVisitor<'f> {
    f: &'f mut Formatter<'f>,
    result: fmt::Result,
}

impl<'f> IrFormatVisitor<'f> {
    fn new(f: &'f mut Formatter<'f>) -> Self {
        Self { f, result: Ok(()) }
    }
}

impl IrVisitor for IrFormatVisitor<'_> {
    fn visit_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) {
        self.result = write!(self.f, "{}:", bb);
        visit::walk_basic_block(self, bb_data);
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        self.result = write!(self.f, "\t{}", instr);
    }
}
