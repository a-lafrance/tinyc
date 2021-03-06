use std::{
    collections::HashSet,
    io::{self, Write},
};
use crate::utils::take_result;
use super::{
    isa::{BasicBlock, BasicBlockData, Body, ControlFlowEdge, Instruction},
    visit::IrVisitor,
};

pub type FmtResult = io::Result<()>;

pub struct IrFormatter<W: Write>(IrFormat<W>);

impl<W: Write> IrFormatter<W> {
    pub fn new(fmt: IrFormat<W>) -> IrFormatter<W> {
        IrFormatter(fmt)
    }

    pub fn fmt(&mut self, name: &str, body: &Body) -> FmtResult {
        self.0.write_prologue(name)?;

        if let Some(root) = body.root_block() {
            self.fmt_basic_block(root, body, &mut HashSet::new())?;
        }

        self.0.write_epilogue()
    }

    fn fmt_basic_block(&mut self, bb: BasicBlock, body: &Body, visited: &mut HashSet<BasicBlock>) -> FmtResult {
        if !visited.contains(&bb) {
            visited.insert(bb);
            let bb_data = self.fmt_single_basic_block(bb, body)?;

            match bb_data.edge() {
                ControlFlowEdge::Fallthrough(dest) => self.fmt_basic_block(dest, body, visited)?,
                ControlFlowEdge::Branch(dest) => self.fmt_basic_block(dest, body, visited)?,
                ControlFlowEdge::IfStmt(then_bb, else_bb, join_bb) => {
                    // add join to visited
                    visited.insert(join_bb);

                    // walk then path until join
                    self.fmt_basic_block(then_bb, body, visited)?;

                    // walk else path until join
                    if let Some(else_bb) = else_bb {
                        self.fmt_basic_block(else_bb, body, visited)?;
                    }

                    // rm join from visited, walk join path
                    visited.remove(&join_bb);
                    self.fmt_basic_block(join_bb, body, visited)?;
                },
                ControlFlowEdge::Loop(body_bb, follow_bb) => {
                    self.fmt_basic_block(body_bb, body, visited)?;
                    self.fmt_basic_block(follow_bb, body, visited)?;
                },
                ControlFlowEdge::Leaf => (),
            }
        }

        Ok(())
    }

    /// This function only exists to make error handling nicer
    fn fmt_single_basic_block<'ir>(&mut self, bb: BasicBlock, body: &'ir Body) -> io::Result<&'ir BasicBlockData> {
        let bb_data = body.basic_block_data(bb);
        self.0.write_basic_block(bb, bb_data).map(|_| bb_data)
    }
}

trait IrWriter {
    fn write_prologue(&mut self, body_name: &str) -> FmtResult;
    fn write_epilogue(&mut self) -> FmtResult;
    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult;
}

pub enum IrFormat<W: Write> {
    Text(TextWriter<W>),
    Graph(GraphWriter<W>),
}

impl<W: Write> IrWriter for IrFormat<W> {
    fn write_prologue(&mut self, body_name: &str) -> FmtResult {
        match self {
            IrFormat::Text(wr) => wr.write_prologue(body_name),
            IrFormat::Graph(wr) => wr.write_prologue(body_name),
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
    fn write_prologue(&mut self, body_name: &str) -> FmtResult {
        writeln!(self.0, "@{}:", body_name)
    }

    fn write_epilogue(&mut self) -> FmtResult {
        writeln!(self.0)
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
            self.1 = writeln!(self.0, "    {}", instr);
        }
    }
}


pub struct GraphWriter<W: Write>(W, FmtResult);

impl<W: Write> GraphWriter<W> {
    pub fn new(wr: W) -> GraphWriter<W> {
        GraphWriter(wr, Ok(()))
    }

    fn emit_fallthrough(&mut self, src: BasicBlock, dest: BasicBlock) -> FmtResult {
        writeln!(self.0, "{}:s -> {}:n [label=\"fallthrough\"];", src, dest)
    }

    fn emit_branch(&mut self, src: BasicBlock, dest: BasicBlock) -> FmtResult {
        writeln!(self.0, "{}:s -> {}:n [label=\"branch\"];", src, dest)
    }
}

impl<W: Write> IrWriter for GraphWriter<W> {
    fn write_prologue(&mut self, body_name: &str) -> FmtResult {
        writeln!(self.0, "digraph {} {{", body_name)
    }

    fn write_epilogue(&mut self) -> FmtResult {
        writeln!(self.0, "}}")
    }

    fn write_basic_block(&mut self, bb: BasicBlock, bb_data: &BasicBlockData) -> FmtResult {
        write!(self.0, "{0} [shape=record, label=\"<b>{0} | {{", bb)?;

        self.visit_basic_block(bb, bb_data);
        take_result(&mut self.1)?;

        writeln!(self.0, "}}\"];")?;

        match bb_data.edge() {
            ControlFlowEdge::Leaf => (),
            ControlFlowEdge::Fallthrough(dest_bb) => self.emit_fallthrough(bb, dest_bb)?,
            ControlFlowEdge::Branch(dest_bb) => self.emit_branch(bb, dest_bb)?,
            ControlFlowEdge::IfStmt(then_bb, else_bb, _) => {
                self.emit_fallthrough(bb, then_bb)?;

                if let Some(else_bb) = else_bb {
                    self.emit_branch(bb, else_bb)?;
                }
            },
            ControlFlowEdge::Loop(body_bb, follow_bb) => {
                self.emit_fallthrough(bb, body_bb)?;
                self.emit_branch(bb, follow_bb)?;
            },
        }

        if let Some(dominator) = bb_data.dominator() {
            writeln!(self.0, "{}:b -> {}:b [color=blue, style=dotted, label=\"dom\"]", dominator, bb)?;
        }

        Ok(())
    }
}

impl<W: Write> IrVisitor for GraphWriter<W> {
    fn visit_basic_block(&mut self, _: BasicBlock, bb_data: &BasicBlockData) {
        // manually enumerate through the statements and emit stuff for them
        // visit first instr
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
