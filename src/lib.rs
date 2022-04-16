#![allow(clippy::needless_collect)] // because of the false positives

pub(crate) mod ast;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod semcheck;
pub(crate) mod sym;
pub(crate) mod tok;
pub(crate) mod utils;

use std::{
    error::Error,
    ffi::OsString,
    fmt::{self, Display, Formatter},
    fs::File,
    io::{self, Read, Write},
    str::FromStr,
};
use self::{
    ir::{fmt::{GraphWriter, IrFormat, IrFormatter, TextWriter}, IrStore},
    parser::Parser,
};
use clap::Parser as ArgParse;

#[derive(Debug, ArgParse)]
#[clap(author, version, about)]
struct Config {
    #[clap(help = "The source file to compile")]
    input: String,

    #[clap(short, long, help = "The output file path")]
    output: Option<String>,

    #[clap(long, help = "Format to dump generated IR")]
    dump_ir: Option<IrDumpFormat>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum IrDumpFormat {
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
struct InvalidDumpFormat(String);

impl Display for InvalidDumpFormat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for InvalidDumpFormat { }

fn make_ir_dump_fmt<W: Write>(dump_fmt: IrDumpFormat, wr: W) -> IrFormat<W> {
    match dump_fmt {
        IrDumpFormat::Text => IrFormat::Text(TextWriter(wr)),
        IrDumpFormat::Graph => IrFormat::Graph(GraphWriter(wr)),
    }
}

fn dump_ir<W: Write>(dump_fmt: IrDumpFormat, wr: W, ir: &IrStore) -> io::Result<()> {
    IrFormatter::new(make_ir_dump_fmt(dump_fmt, wr)).fmt(ir)
}


pub fn start<Args, T>(args: Args)
where
    Args: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let config = Config::parse_from(args);
    let mut src_file = File::open(config.input).expect("failed to open input file");

    let mut input = String::new();
    src_file
        .read_to_string(&mut input)
        .expect("failed to read source file");

    let tokens = scanner::tokenize(&input);

    match Parser::new(tokens).and_then(|mut p| p.parse_computation()) {
        Ok(ast) => {
            let ir = IrStore::from(ast);

            if let Some(dump_fmt) = config.dump_ir {
                // FIXME: better error handling when opening outfile
                let mut outfile = config.output.map(|f| File::create(f).expect("failed to open output file"));
                let dump_result = match outfile.as_mut() {
                    Some(outfile) => dump_ir(dump_fmt, outfile, &ir),
                    None => dump_ir(dump_fmt, &mut io::stdout(), &ir),
                };

                dump_result.expect("failed to dump IR");
            }
        },

        Err(e) => eprintln!("\x1b[31merror\x1b[0m: {}", e),
    };
}
