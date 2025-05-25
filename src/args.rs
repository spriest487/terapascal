use std::{
    path::PathBuf,
};
use structopt::*;
use terapascal_common::LanguageMode;

#[derive(StructOpt, Debug)]
pub struct Args {
    /// source file of program/library main unit
    #[structopt(name = "FILE", parse(from_os_str))]
    pub file: PathBuf,

    /// Output file path.
    /// If the output file extension matches a backend, the output from that backend will be written
    /// to this path.
    /// If no output path is provided the vm will be invoked.
    #[structopt(name = "OUTPUT", short = "o", parse(from_os_str))]
    pub output: Option<PathBuf>,

    #[structopt(long="define", short = "d")]
    pub define_syms: Vec<String>,

    #[structopt(long="mode", short="m", default_value = "default", parse(try_from_str = parse_lang_mode))]
    pub lang_mode: LanguageMode,

    /// additional units to compile
    #[structopt(long = "units", short = "u")]
    pub units: Vec<String>,

    /// source dir for unit source files
    #[structopt(long = "search-dir", short = "s", parse(from_os_str))]
    pub search_dirs: Vec<PathBuf>,

    /// if set, run compilation to a given stage and print the output as human-readable
    /// text instead of creating an output file
    #[structopt(short = "p", long = "print-stage", parse(try_from_str = parse_compile_stage))]
    pub print_stage: Option<CompileStage>,

    /// vm: log RC heap usage
    #[structopt(long = "trace-heap")]
    pub trace_heap: bool,

    /// vm: log RC retain/release operations
    #[structopt(long = "trace-rc")]
    pub trace_rc: bool,

    /// vm: log executed IR instructions
    #[structopt(long = "trace-ir")]
    pub trace_ir: bool,

    /// print frontend backtrace on compilation failure
    #[structopt(long = "backtrace", short = "bt")]
    pub backtrace: bool,

    #[structopt(long = "verbose", short = "v")]
    pub verbose: bool,

    #[structopt(short = "g", long = "debug")]
    pub debug: bool,

    #[structopt(long = "gcodeview")]
    pub debug_codeview: bool,
    
    #[structopt(long = "diag-port", default_value = "0")]
    pub diag_port: u16,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum CompileStage {
    Preprocess,
    Parse,
    Typecheck,
    EmitIR,
}

fn parse_compile_stage(s: &str) -> Result<CompileStage, String> {
    match s {
        "ir" | "codegen-ir" => Ok(CompileStage::EmitIR),
        "p" | "parse" => Ok(CompileStage::Parse),
        "t" | "typecheck" => Ok(CompileStage::Typecheck),
        "pp" | "preprocess" => Ok(CompileStage::Preprocess),
        _ => Err(format!("invalid output kind: {}", s)),
    }
}

fn parse_lang_mode(s: &str) -> Result<LanguageMode, String> {
    match s {
        "delphi" | "Delphi" => Ok(LanguageMode::Delphi),
        "fpc" | "FPC" => Ok(LanguageMode::Fpc),
        "default" | "Default" => Ok(LanguageMode::Default),
        _ => Err(format!("invalid language mode: {}", s)),
    }
}
