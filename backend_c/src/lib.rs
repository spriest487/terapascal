mod rtti;
pub mod ast;
pub use terapascal_ir as ir;

pub use ast as c;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
    
    pub debug: bool,
}

pub fn translate(lib: &ir::Library, opts: Options) -> c::Unit {
    let mut unit = c::Unit::new(&lib.metadata(), opts);
    unit.add_lib(lib);

    unit
}
