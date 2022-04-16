use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    io::Write,
    str::FromStr,
};
use crate::ir::{
    fmt::{FmtResult, GraphWriter, IrFormat, IrFormatter, TextWriter},
    IrStore,
};

pub fn dump_ir<W: Write>(dump_fmt: IrDumpFormat, wr: W, ir: &IrStore) -> FmtResult {
    IrFormatter::new(make_ir_dump_fmt(dump_fmt, wr)).fmt(ir)
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IrDumpFormat {
    Text, Graph
}

impl FromStr for IrDumpFormat {
    type Err = InvalidDumpFormat;

    fn from_str(s: &str) -> Result<IrDumpFormat, Self::Err> {
        match s {
            "text" => Ok(IrDumpFormat::Text),
            "graph" => Ok(IrDumpFormat::Graph),
            other => Err(InvalidDumpFormat(other.to_string())),
        }
    }
}

#[derive(Debug)]
pub struct InvalidDumpFormat(String);

impl Display for InvalidDumpFormat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for InvalidDumpFormat { }


fn make_ir_dump_fmt<W: Write>(dump_fmt: IrDumpFormat, wr: W) -> IrFormat<W> {
    match dump_fmt {
        IrDumpFormat::Text => IrFormat::Text(TextWriter::new(wr)),
        IrDumpFormat::Graph => IrFormat::Graph(GraphWriter::new(wr)),
    }
}
