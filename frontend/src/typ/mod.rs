mod annotation;
mod context;
mod result;

pub mod ast;
pub mod ty;

#[cfg(test)]
pub mod test;

pub use self::annotation::*;
pub use self::context::*;
pub use self::result::*;
pub use self::ty::*;
use crate::ast::Unit;
use ast::typecheck_unit;
use std::path::PathBuf;
use terapascal_common::build_log::BuildLog;

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub path: PathBuf,
    
    pub unit: ast::Unit,
    pub context: Context,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub units: Vec<ModuleUnit>,
    pub root_ctx: Box<Context>,
}

impl Module {
    pub fn typecheck<'a>(units: impl DoubleEndedIterator<Item=(&'a PathBuf, &'a Unit)>, verbose: bool, log: &mut BuildLog) -> TypeResult<Self> {
        // eprintln!("function sig size: {}", std::mem::size_of::<sig::FunctionSig>());
        // eprintln!("type size: {}", std::mem::size_of::<Type>());
        // eprintln!("type annotation size: {}", std::mem::size_of::<TypeAnnotation>());
        // eprintln!("expr size: {}", std::mem::size_of::<ast::Expression>());
        // eprintln!("stmt size: {}", std::mem::size_of::<ast::Statement>());
        // eprintln!("type list size: {}", std::mem::size_of::<TypeList>());
        // eprintln!("ident size: {}", std::mem::size_of::<frontend::Ident>());
        // eprintln!("ident path size: {}", std::mem::size_of::<IdentPath>());
        // eprintln!("span size: {}", std::mem::size_of::<Span>());

        let mut root_ctx = Context::root();
        let mut module_units = Vec::new();

        // typecheck in reverse order - the parsing order starts with the root unit, but we
        // need to typecheck dependencies first
        for (unit_path, unit) in units.into_iter().rev() {
            if verbose {
                log.trace(format!("Typechecking {} {}", unit.kind, unit.ident));
            }

            module_units.push(typecheck_unit(unit_path, unit, &mut root_ctx)?);
        }
        
        module_units.reverse();

        Ok(Module {
            units: module_units,
            root_ctx: Box::new(root_ctx),
        })
    }
    
    pub fn main_unit(&self) -> &ModuleUnit {
        &self.units[0]
    }
}
