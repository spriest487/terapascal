use terapascal_ir::{Library, Metadata};
use crate::{Interpreter, InterpreterOpts};

#[test]
fn empty_library_executes_ok() {
    let metadata = Metadata::new();
    let lib = Library::new(metadata);
    
    let opts = InterpreterOpts {
        verbose: true,
        ..Default::default()
    };

    let mut vm = Interpreter::new(opts);

    vm.load_lib(&lib).expect("should load successfully");
}
