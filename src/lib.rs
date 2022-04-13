#![allow(clippy::needless_collect)] // because of the false positives

pub(crate) mod ast;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod semcheck;
pub(crate) mod sym;
pub(crate) mod tok;
pub(crate) mod utils;

use self::{
    ir::IrStore,
    parser::Parser,
};
use clap::Parser as ArgParse;
use std::{ffi::OsString, fs::File, io::Read};

#[derive(Debug, ArgParse)]
#[clap(author, version, about)]
struct Config {
    #[clap(help = "The source file to compile")]
    input: String,

    #[clap(short, long, default_value = "out", help = "The output file path")]
    output: String,
    // Add a --dump-ir flag that specifies to dump the IR, which can be in either
    // text (ie weird assembly-ish text) or graph (ie dot graph) format
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
            println!("generated ir: {:?}", ir);
        },

        Err(e) => eprintln!("\x1b[31merror\x1b[0m: {}", e),
    };
}
