mod rtti;
pub mod ast;
pub use terapascal_ir as ir;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
    
    pub debug: bool,
    
    pub enable_rtti: bool,
}

pub fn translate(lib: &ir::Library, opts: Options) -> ast::Unit<'_> {
    let mut unit = ast::Unit::new(&lib.metadata(), opts);
    unit.add_lib(lib);

    unit
}
