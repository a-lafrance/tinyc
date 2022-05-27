mod dump;
pub(crate) mod opt;

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
use self::{
    dump::{dump_ir, IrDumpFormat},
    opt::{OptConfig, OptLevel, RegAllocator},
};

#[derive(Debug, ArgParse)]
#[clap(author, version, about)]
pub(crate) struct Config {
    #[clap(help = "The source file to compile")]
    pub input: String,

    #[clap(short, long, help = "The output file path")]
    pub output: Option<String>,

    #[clap(long, help = "Emit IR in the specified format")]
    pub dump_ir: Option<IrDumpFormat>,

    #[clap(short, long, help = "Architecture for which to emit native code")]
    pub arch: Option<SupportedArch>,

    #[clap(short = 'O', long = "opt", default_value_t, help = "Optimization level to enable")]
    pub opt_level: OptLevel,

    #[clap(long, help = "Manually enable common subexpression elimination")]
    pub enable_cse: bool,

    #[clap(long, help = "Manually enable constant propagation")]
    pub enable_const_prop: bool,

    #[clap(long, help = "Manually enable dead code elimination")]
    pub enable_dead_code_elim: bool,

    #[clap(long, help = "Manually enable instruction selection")]
    pub enable_instr_select: bool,

    #[clap(long, help = "Manually select the register allocator to use")]
    pub reg_alloc: Option<RegAllocator>,
}

pub fn start<Args, T>(args: Args)
where
    Args: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let config = Config::parse_from(args);
    let opt = OptConfig::from(&config);

    let mut src_file = File::open(config.input).expect("failed to open input file");
    let mut input = String::new();
    src_file
        .read_to_string(&mut input)
        .expect("failed to read source file");

    let tokens = scanner::tokenize(&input);

    match Parser::new(tokens).and_then(|mut p| p.parse_computation()) {
        Ok(ast) => {
            let ir = IrStore::from_ast(ast, opt);

            if let Some(dump_fmt) = config.dump_ir {
                // FIXME: better error handling when opening outfile
                let mut outfile = config.output.map(|f| File::create(f).expect("failed to open output file"));
                let dump_result = match outfile.as_mut() {
                    Some(outfile) => dump_ir(dump_fmt, outfile, &ir),
                    None => dump_ir(dump_fmt, &mut io::stdout(), &ir),
                };

                dump_result.expect("failed to dump IR");

                if config.arch.is_some() {
                    println!("warning: --arch is ignored when --dump-ir is provided");
                }
            } else if let Some(SupportedArch::Dlx) = config.arch {
                let outfile = File::create(config.output.unwrap_or_else(|| "a.out".to_string()))
                    .expect("failed to open output file");
                dlx::gen_code(ir, BufWriter::new(outfile), opt);
            }
        },

        Err(e) => eprintln!("\x1b[31merror\x1b[0m: {}", e),
    };
}
