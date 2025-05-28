use terapascal_common::span::Spanned;
use crate::ast;
use crate::typ::ast::InterfaceDecl;
use crate::typ::test::module_from_src;
use crate::typ::test::try_module_from_src;
use crate::typ::{builtin_string_name, Module, Primitive, Type};
use crate::typ::NameError;
use crate::typ::TypeError;
use crate::typ::TypeResult;

fn get_decl_iface<'a>(module: &'a Module, unit_name: &str, name: &str) -> &'a InterfaceDecl {
    let unit = module.units.iter()
        .find(|u| u.unit.ident.to_string() == unit_name)
        .unwrap();

    unit.unit
        .type_decl_items()
        .find_map(|(_, item)| {
            if item.name().full_path.last().as_str() != name {
                return None;
            }

            match item {
                ast::TypeDeclItem::Interface(iface_decl) => Some(iface_decl),
                _ => None,
            }})
        .unwrap()
        .as_ref()
}

#[test]
fn self_ty_is_valid_in_interface() {
    let module = module_from_src(
        "test", 
        r"
        implementation

        type MyInterface = interface
            function A(a: Self): Self;
        end;

        end."
    );
    
    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");    
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("Self", method_a.decl.params[1].ty.to_string());
    assert_eq!("Self", method_a.decl.result_ty.to_string());
}

#[test]
fn self_ty_is_valid_in_interface_sig_with_arrays() {
    let module = module_from_src(
        "test",
        r"implementation
        
        type MyInterface = interface
            function A(a: array of Self): array[2] of Self;
        end;
        
        end."
    );

    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("array of Self", method_a.decl.params[1].ty.to_string());
    assert_eq!("array[2] of Self", method_a.decl.result_ty.to_string());
}

#[test]
fn self_ty_is_valid_in_interface_sig_with_generics() {
    let module = module_from_src(
        "test",
        r"
        implementation
        
        type MyGeneric[T] = class end;

        type MyInterface = interface
            function A(a: MyGeneric[Self]): MyGeneric[Self];
        end;
        
        end."
    );

    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("test.MyGeneric[Self]", method_a.decl.params[1].ty.to_string());
    assert_eq!("test.MyGeneric[Self]", method_a.decl.result_ty.to_string());
}

fn expect_self_not_found_err(result: TypeResult<Module>) {
    match result {
        Ok(..) => panic!("expected type error, but got success"),
        Err(TypeError::NameError { err: NameError::NotFound { ident, .. }, .. }) => {
            assert_eq!("Self", ident.to_string());
        }
        Err(err) => panic!("expected NotFound for Self, got:\n{}\n{}", err, err.span())
    }
}

#[test]
fn self_ty_not_valid_in_class() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MyClass = class
            function A: Self;
        end;

        end."
    );

    expect_self_not_found_err(result);
}

#[test]
fn self_ty_not_valid_in_variant() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MyVariant = variant
            A: Self;
        end;
        
        end."
    );

    expect_self_not_found_err(result);
}

#[test]
fn set_decl_with_mixed_item_types_is_invalid() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MySet = set of 1 as Integer..2 as Byte;
        
        end.
        "
    );

    match result {
        Ok(..) => panic!("expected error"),
        Err(TypeError::TypeMismatch { expected, actual, .. }) => {
            assert_eq!(Type::Primitive(Primitive::Int32), expected);
            assert_eq!(Type::Primitive(Primitive::UInt8), actual);
        }
        Err(err) => panic!("expected type mismatch error, got: {}", err)
    }
}

#[test]
fn set_decl_with_non_numeric_types_is_invalid() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MySet = set of 'hello'..'world';
        
        end.
        "
    );

    match result {
        Ok(..) => panic!("expected error"),
        Err(TypeError::InvalidSetValueType { actual, .. }) => {
            let string_ty = Type::class(builtin_string_name());
            assert_eq!(string_ty, actual);
        }
        Err(err) => panic!("expected set values must be numeric error, got: {}", err)
    }
}

#[test]
fn set_decl_with_characters_is_valid() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MySet = set of 'a'..'c';
        
        end.
        "
    );

    if let Err(err) = result {
        panic!("{}", err)
    }
}


#[test]
fn set_decl_with_enum_members_is_valid() {
    let result = try_module_from_src(
        "test",
        r"
        implementation
        
        type MyEnum = (NumOne, NumTwo, NumThree);
        type MySet = set of NumOne..NumThree;

        end.
        "
    );

    if let Err(err) = result {
        panic!("{}", err)
    }
}
