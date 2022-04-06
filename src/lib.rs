pub(crate) mod ast;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod tok;
pub(crate) mod utils;

use self::parser::Parser;
use clap::Parser as ArgParse;
use std::{ffi::OsString, fs::File, io::Read};

#[derive(Debug, ArgParse)]
#[clap(author, version, about)]
struct Config {
    #[clap(help = "The source file to compile")]
    input: String,

    #[clap(short, long, default_value = "out", help = "The output file path")]
    output: String,
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

    match Parser::new(tokens).parse_computation() {
        Ok(ast) => println!("{:?}", ast),
        Err(e) => eprintln!("parse error: {}", e),
    };
}
