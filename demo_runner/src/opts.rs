use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
pub struct Opts {
    #[structopt(name = "FILE", parse(from_os_str))]
    pub search_path: PathBuf,
    
    #[structopt(long = "target", parse(from_os_str))]
    pub target_path: PathBuf,

    #[structopt(long, parse(from_os_str))]
    pub compiler: PathBuf,
    
    #[structopt(long)]
    pub debug: bool,

    #[structopt(long, short = "v")]
    pub verbose: bool,

    #[structopt(long)]
    pub clean: bool,

    #[structopt(long = "continue")]
    pub error_continue: bool,

    #[structopt(long, short, default_value = "vm", parse(try_from_str))]
    pub exec: ExecutionMethod, 
}

#[derive(Debug, Copy, Clone)]
pub enum ExecutionMethod {
    Vm,
    Dotnet,
    Clang,
}

impl FromStr for ExecutionMethod {
    type Err = String;

    fn from_str(s: &str) -> Result<ExecutionMethod, String> {
        match s {
            "clang" => Ok(ExecutionMethod::Clang),
            "vm" => Ok(ExecutionMethod::Vm),
            "dotnet" => Ok(ExecutionMethod::Dotnet),
            _ => Err(s.to_string())
        }
    }
}
