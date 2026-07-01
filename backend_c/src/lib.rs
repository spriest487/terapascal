pub mod c;
pub use terapascal_ir as ir;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Options {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
    pub trace_generics: bool,
    
    pub debug: bool,
    
    pub enable_rtti: bool,
}

pub fn translate(lib: &ir::Library, opts: Options) -> c::Unit<'_> {
    let mut unit = c::Unit::new(&lib.metadata(), opts);
    unit.add_lib(lib);

    unit
}
