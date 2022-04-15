#![allow(clippy::needless_collect)] // because of the false positives

pub(crate) mod ast;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod semcheck;
pub(crate) mod sym;
pub(crate) mod tok;
pub(crate) mod utils;

use std::{ffi::OsString, fs::File, io::{self, Read}};
use self::{
    ir::{fmt::IrFormatter, IrStore},
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

    #[clap(long)]
    dump_ir: bool,
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

            if config.dump_ir {
                // FIXME: better error handling when opening outfile
                let mut outfile = config.output.map(|f| File::create(f).expect("failed to open output file"));
                let fmt_result = match outfile.as_mut() {
                    Some(outfile) => IrFormatter::fmt(outfile, &ir),
                    None => IrFormatter::fmt(&mut io::stdout(), &ir),
                };

                fmt_result.expect("failed to dump IR");
            }
        },

        Err(e) => eprintln!("\x1b[31merror\x1b[0m: {}", e),
    };
}
