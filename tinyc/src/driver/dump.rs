use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    io::Write,
    str::FromStr,
};
use crate::{
    codegen::{dlx, SupportedArch},
    ir::{
        fmt::{FmtResult, GraphWriter, IrFormat, IrFormatter, TextWriter},
        IrStore,
    },
};
use super::OptConfig;

pub fn dump_from_ir<W: Write>(
    fmt: DumpFormat,
    wr: W,
    ir: IrStore,
    arch: Option<SupportedArch>,
    opt: OptConfig,
) -> FmtResult {
    match fmt {
        DumpFormat::Ir => {
            let mut f = IrFormatter::new(IrFormat::Text(TextWriter::new(wr)));

            for (name, body) in ir.bodies() {
                f.fmt(name, body)?;
            }
        }

        DumpFormat::IrCfg => {
            let mut f = IrFormatter::new(IrFormat::Graph(GraphWriter::new(wr)));

            for (name, body) in ir.bodies() {
                f.fmt(name, body)?;
            }
        }

        DumpFormat::Asm => match arch {
            Some(SupportedArch::Dlx) => dlx::gen_asm(ir, wr, opt),
            None => eprintln!("error: --arch must be specified to dump assembly"),
        }
    }

    Ok(())
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DumpFormat {
    Ir, IrCfg, Asm,
}

impl FromStr for DumpFormat {
    type Err = InvalidDumpFormat;

    fn from_str(s: &str) -> Result<DumpFormat, Self::Err> {
        match s {
            "ir" => Ok(DumpFormat::Ir),
            "ir-cfg" => Ok(DumpFormat::IrCfg),
            "asm" => Ok(DumpFormat::Asm),
            other => Err(InvalidDumpFormat(other.to_string())),
        }
    }
}

#[derive(Debug)]
pub struct InvalidDumpFormat(String);

impl Display for InvalidDumpFormat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "invalid IR dump format '{}'", self.0)
    }
}

impl Error for InvalidDumpFormat { }
