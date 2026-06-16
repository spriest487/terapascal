use std::path::Path;
use std::path::PathBuf;
use structopt::*;
use terapascal_build::BuildStage;
use terapascal_common::version::Version;
use terapascal_common::LanguageMode;

#[derive(StructOpt, Debug)]
pub struct Args {
    /// Path to the source file of the program or library unit to compile
    #[structopt(name = "FILE", parse(from_os_str))]
    pub file: PathBuf,

    /// Output file path.
    /// If the output file extension matches a backend, the output from that backend will be written
    /// to this path.
    /// If no output path is provided the vm will be invoked.
    #[structopt(short = "o", parse(from_os_str))]
    pub output: Option<PathBuf>,

    #[structopt(name = "project-name")]
    pub project_name: Option<String>,

    #[structopt(name = "project-version")]
    pub project_version: Option<Version>,

    #[structopt(
        name = "arch",
        short = "a",
        default_value = "native",
        parse(try_from_str = parse_target_arch),
    )]
    pub arch: TargetArch,

    /// Additional symbols to define in all compiled units
    #[structopt(long="define", short = "d")]
    pub define_syms: Vec<String>,

    #[structopt(long="mode", default_value = "default", parse(try_from_str = parse_lang_mode))]
    pub lang_mode: LanguageMode,

    /// If false, runtime type information objects will not be generated
    #[structopt(long="rtti", default_value = "true", parse(try_from_str = parse_bool))]
    pub rtti: bool,

    /// If false, code using `unsafe` blocks will produce an error
    #[structopt(long="unsafe", default_value = "true", parse(try_from_str = parse_bool))]
    pub allow_unsafe: bool,

    /// Source directory to search for unit source files
    #[structopt(long = "search-dir", short = "s", parse(from_os_str))]
    pub search_dirs: Vec<PathBuf>,

    /// Add a package dependency on a package with the given name
    #[structopt(long = "package", short = "p")]
    pub packages: Vec<String>,

    /// Disable the automatic reference to the "System" package, e.g. for compiling the system
    /// package itself.
    #[structopt(long = "no-system")]
    pub no_system: bool,

    /// If set, run compilation to the given stage and print a representation of the output to
    /// stdout as text instead of creating an output file
    #[structopt(long = "dump", parse(try_from_str = parse_build_stage_arg))]
    pub dump_stage: Option<BuildStage>,

    /// Log RC heap usage to stderr
    #[structopt(long = "trace-heap")]
    pub trace_heap: bool,

    /// Log RC retain/release operations to stderr
    #[structopt(long = "trace-rc")]
    pub trace_rc: bool,

    /// VM: log all executed IR instructions
    #[structopt(long = "trace-ir")]
    pub trace_ir: bool,

    /// Log runtime generic instantiations, where supported by the backend
    #[structopt(long = "trace-generics")]
    pub trace_generics: bool,

    /// Print compiler backtrace on certain compilation errors
    #[structopt(long = "backtrace", short = "bt")]
    pub backtrace: bool,

    /// Print extra information during the compilation process
    #[structopt(long = "verbose", short = "v")]
    pub verbose: bool,

    /// Preserve debug information, names, and comments in compiled objects
    #[structopt(short = "g", long = "debug")]
    pub debug: bool,

    /// Clang only: compile with -g-codeview for Visual Studio debugging
    #[allow(unused)]
    #[structopt(long = "gcodeview")]
    pub debug_codeview: bool,

    /// VM only: if the debug_server feature is enabled, run an HTTP server serving debug
    /// information about the current execution state on localhost at this port
    #[allow(unused)]
    #[structopt(long = "diag-port", default_value = "0")]
    pub diag_port: u16,
}

impl Args {
    pub fn output_path(&self) -> Option<&Path> {
        self.output.as_ref().map(PathBuf::as_path)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TargetArch {
    Native,
    Cil,
}

fn parse_target_arch(s: &str) -> Result<TargetArch, String> {
    match s {
        "cil" => Ok(TargetArch::Cil),
        "native" => Ok(TargetArch::Native),
        _ => Err(format!("invalid architecture name: {}", s)),
    }
}

fn parse_build_stage_arg(s: &str) -> Result<BuildStage, String> {
    match s {
        "ir" | "codegen-ir" => Ok(BuildStage::Codegen),
        "p" | "parse" => Ok(BuildStage::Parse),
        "t" | "typecheck" => Ok(BuildStage::Typecheck),
        "pp" | "preprocess" => Ok(BuildStage::Preprocess),
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

fn parse_bool(s: &str) -> Result<bool, String> {
    match s {
        "true" | "on" => Ok(true),
        "false" | "off" => Ok(false),
        _ => Err(format!("invalid boolean value: {s}")),
    }
}
