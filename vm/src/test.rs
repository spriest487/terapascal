use terapascal_ir::{Library, Metadata};
use crate::{Vm, ExecOpts};

#[test]
fn empty_library_executes_ok() {
    let metadata = Metadata::new();
    let lib = Library::new(metadata);
    
    let opts = ExecOpts {
        verbose: true,
        ..Default::default()
    };

    let mut vm = Vm::new(opts);

    vm.load_lib(&lib).expect("should load successfully");
}

#[test]
fn created_string_has_expected_content() {
    let opts = ExecOpts {
        verbose: true,
        ..Default::default()
    };

    let mut vm = Vm::new(opts);

    let greeting = "hello, world";

    let string_ptr = vm.create_string(greeting, false).unwrap();
    assert!(string_ptr.as_pointer().is_some());

    let result = vm.read_string_at(string_ptr.as_pointer().unwrap()).unwrap();
    assert_eq!(greeting, result);
}
