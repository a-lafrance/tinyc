use std::ffi::OsString;

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
    println!("{:?}", config);
    
    // TODO: run the compiler
}
