mod dump;

use std::{
    ffi::OsString,
    fs::File,
    io::{self, BufWriter, Read},
};
use clap::Parser as ArgParse;
use crate::{
    codegen::{dlx, SupportedArch},
    ir::IrStore,
    parser::Parser,
    scanner,
};
use self::dump::{dump_ir, IrDumpFormat};

#[derive(Debug, ArgParse)]
#[clap(author, version, about)]
struct Config {
    #[clap(help = "The source file to compile")]
    input: String,

    #[clap(short, long, help = "The output file path")]
    output: Option<String>,

    #[clap(long, help = "Format to dump generated IR")]
    dump_ir: Option<IrDumpFormat>,

    #[clap(short, long, help = "Architecture to emit native code for")]
    arch: Option<SupportedArch>,
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
            } else if let Some(SupportedArch::Dlx) = config.arch {
                let outfile = File::create(config.output.unwrap_or_else(|| "a.out".to_string()))
                    .expect("failed to open output file");
                dlx::gen_code(ir, BufWriter::new(outfile));
            }
        },

        Err(e) => eprintln!("\x1b[31merror\x1b[0m: {}", e),
    };
}
