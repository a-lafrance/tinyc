use std::{
    collections::HashSet,
    io::{self, Write},
};
use crate::utils::take_result;
use super::{
    isa::{BasicBlock, BasicBlockData, Instruction},
    visit::IrVisitor,
    IrStore,
};

pub type FmtResult = io::Result<()>;

pub struct IrFormatter<W: Write>(IrFormat<W>);

impl<W: Write> IrFormatter<W> {
    pub fn new(fmt: IrFormat<W>) -> IrFormatter<W> {
        IrFormatter(fmt)
    }

    pub fn fmt(&mut self, ir: &IrStore) -> FmtResult {
        self.0.write_prologue()?;

        if let Some(root) = ir.root_block() {
            self.fmt_basic_block(root, ir, &mut HashSet::new())?;
        }

        self.0.write_epilogue()
    }

    fn fmt_basic_block(&mut self, bb: BasicBlock, ir: &IrStore, visited: &mut HashSet<BasicBlock>) -> FmtResult {
        if !visited.contains(&bb) {
            visited.insert(bb);
            let bb_data = self.fmt_single_basic_block(bb, ir)?;

            if let Some(fallthrough_bb) = bb_data.fallthrough_dest() {
                self.fmt_basic_block(fallthrough_bb, ir, visited)?;
            }

            if let Some(branch_bb) = bb_data.branch_dest() {
                self.fmt_basic_block(branch_bb, ir, visited)?;
            }
        }

        Ok(())
    }

    /// This function only exists to make error handling nicer
    fn fmt_single_basic_block<'ir>(&mut self, bb: BasicBlock, store: &'ir IrStore) -> io::Result<&'ir BasicBlockData> {
        let bb_data = store.basic_block_data(bb);
        self.0.write_basic_block(bb, bb_data).map(|_| bb_data)
    }
}

trait IrWriter {
    fn write_prologue(&mut self) -> FmtResult;
    fn write_epilogue(&mut self) -> FmtResult;
    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult;
}

pub enum IrFormat<W: Write> {
    Text(TextWriter<W>),
    Graph(GraphWriter<W>),
}

impl<W: Write> IrWriter for IrFormat<W> {
    fn write_prologue(&mut self) -> FmtResult {
        match self {
            IrFormat::Text(wr) => wr.write_prologue(),
            IrFormat::Graph(wr) => wr.write_prologue(),
        }
    }

    fn write_epilogue(&mut self) -> FmtResult {
        match self {
            IrFormat::Text(wr) => wr.write_epilogue(),
            IrFormat::Graph(wr) => wr.write_epilogue(),
        }
    }

    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult {
        match self {
            IrFormat::Text(wr) => wr.write_basic_block(bb, bb_data),
            IrFormat::Graph(wr) => wr.write_basic_block(bb, bb_data),
        }
    }
}

pub struct TextWriter<W: Write>(W, FmtResult);

impl<W: Write> TextWriter<W> {
    pub fn new(wr: W) -> TextWriter<W> {
        TextWriter(wr, Ok(()))
    }
}

impl<W: Write> IrWriter for TextWriter<W> {
    fn write_prologue(&mut self) -> FmtResult {
        Ok(())
    }

    fn write_epilogue(&mut self) -> FmtResult {
        Ok(())
    }

    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult {
        writeln!(self.0, "{}:", bb)?;
        self.visit_basic_block(bb, bb_data);
        take_result(&mut self.1)?;
        writeln!(self.0)
    }
}

impl<W: Write> IrVisitor for TextWriter<W> {
    fn visit_instr(&mut self, instr: &Instruction) {
        if self.1.is_ok() {
            self.1 = writeln!(self.0, "  {}", instr);
        }
    }
}

pub struct GraphWriter<W: Write>(W, FmtResult);

impl<W: Write> GraphWriter<W> {
    pub fn new(wr: W) -> GraphWriter<W> {
        GraphWriter(wr, Ok(()))
    }
}

impl<W: Write> IrWriter for GraphWriter<W> {
    fn write_prologue(&mut self) -> FmtResult {
        writeln!(self.0, "digraph CFG {{")
    }

    fn write_epilogue(&mut self) -> FmtResult {
        writeln!(self.0, "}}")
    }

    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult {
        write!(self.0, "{0} [shape=record, label=\"<b>{0} | {{", bb)?;
        self.visit_basic_block(bb, bb_data);
        take_result(&mut self.1)?;

        // TODO: chain the results together appropriately
        writeln!(self.0, "}}\"];")?;

        // FIXME: this feels like repeat code from above
        if let Some(fallthrough_bb) = bb_data.fallthrough_dest() {
            writeln!(self.0, "{}:s -> {}:n [label=\"fallthrough\"];", bb, fallthrough_bb)?;
        }

        if let Some(branch_bb) = bb_data.branch_dest() {
            writeln!(self.0, "{}:s -> {}:n [label=\"branch\"];", bb, branch_bb)?;
        }

        Ok(())
    }
}

impl<W: Write> IrVisitor for GraphWriter<W> {
    fn visit_basic_block(&mut self, _: BasicBlock, bb_data: &BasicBlockData) {
        // manually enumerate through the statements and emit stuff for them
        // visit first instr
        // TODO: handle errors throughout
        if let Some(first_instr) = bb_data.body().first() {
            self.visit_instr(first_instr);
        }

        // for rest of instrs, visit
        for instr in bb_data.body().iter().skip(1) {
            if self.1.is_ok() {
                self.1 = write!(self.0, "|");
            }

            self.visit_instr(instr);
        }
    }

    fn visit_instr(&mut self, instr: &Instruction) {
        if self.1.is_ok() {
            self.1 = write!(self.0, "{}", instr);
        }
    }
}
