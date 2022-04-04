#![feature(iterator_try_collect)]

pub(crate) mod ast;
pub(crate) mod parser;
pub(crate) mod scanner;

use std::{ffi::OsString, fs::File, io::Read};
use clap::Parser;

#[derive(Debug, Parser)]
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

    match scanner::tokenize(&input).try_collect::<Vec<_>>() {
        Ok(tokens) => println!("{:?}", tokens),
        Err(e) => eprintln!("{}", e),
    };
}
