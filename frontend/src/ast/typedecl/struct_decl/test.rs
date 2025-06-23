use crate::ast::util::*;
use crate::ast::StructDecl;
use crate::ast::TypeAnnotation;
use crate::ast::TypeDeclItem;
use crate::ast::Unit;
use crate::ast::Visibility;
use crate::parse::ParseError;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::TracedError;

/// empty class and record types should be allowed - only the typechecker should enforce
/// non-empty records 
#[test]
pub fn empty_struct_is_valid() {
    let unit = unit_from_string("empty_struct_is_valid", r"
        implementation
        
        type 
            MyClass = class
            end;
        
            MyRecord = record
            end;
        
        end
    ");
    
    let decls = unit.type_decls().collect::<Vec<_>>();
    assert_eq!(1, decls.len());
    
    let (vis, type_decls) = decls[0];
    assert_eq!(Visibility::Implementation, vis);
    
    assert_eq!(2, type_decls.items.len());
    
    for item in &type_decls.items {
        match item { 
            TypeDeclItem::Struct(struct_def) => {
                assert_eq!(0, struct_def.fields().count());
            }
            
            other => panic!("expected a struct def, found: {}", other),
        }
    }
}

fn get_single_struct_def(unit: &Unit<Span>) -> &StructDecl<Span> {
    match unit.type_decl_items().nth(0).unwrap().1 {
        TypeDeclItem::Struct(struct_def) => struct_def.as_ref(),
        other => panic!("expected a struct def, got {}", other),
    }
}

#[test]
pub fn semicolon_separator_is_valid() {
    let unit = unit_from_string("semicolon_separator_is_valid", r"
        implementation
        
        type MyClass = class
            i: Int32;
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(0, class_def.methods().count());
    assert_eq!(1, class_def.fields().count());

    let field_decl = class_def.fields().nth(0).unwrap();
    assert_eq!("i", field_decl.idents[0].name.as_str());
    assert_eq!("Int32", field_decl.ty.to_string());
}

#[test]
pub fn semicolon_separator_is_optional() {
    let unit = unit_from_string("semicolon_separator_is_optional", r"
        implementation
        
        type MyClass = class
            i: Int32
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(0, class_def.methods().count());
    assert_eq!(1, class_def.fields().count());
    assert_eq!("i", class_def.fields().nth(0).unwrap().idents[0].name.as_str());
    assert_eq!("Int32", &class_def.fields().nth(0).unwrap().ty.to_string());
}

#[test]
pub fn semicolon_separator_is_single() {
    let unit = try_unit_from_string("semicolon_separator_is_optional", r"
        implementation
        
        type MyClass = class
            i: Int32;;
        end;

        end
    ");
    
    let result = unit.map_err(TracedError::into_inner);

    match result {
        Err(ParseError::UnexpectedToken(tt, ..)) => {
            assert!(tt.is_separator(Separator::Semicolon))
        },

        Err(ParseError::UnitWithErrors(err, ..)) => {
            match err.first.err {
                ParseError::UnexpectedToken(..) | ParseError::UnexpectedTokens(..) => {
                }
                
                other => {
                    panic!("wrong error type: should be unexpected token, was: {:#?}", other)
                },
            }
        },
        
        Err(other) => panic!("wrong error type: should be unexpected token, was: {:#?}", other),
        
        Ok(..) => panic!("should fail on the unexpected semicolon token"),
    }
}

#[test]
pub fn multi_field_def_is_valid() {
    let unit = unit_from_string("multi_field_def_is_valid", r"
        implementation
        
        type MyClass = class
            a, b: Int32;
            c: Int32;
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(0, class_def.methods().count());
    assert_eq!(2, class_def.fields().count());

    assert_eq!(2, class_def.fields().nth(0).unwrap().idents.len());
    
    assert_eq!(&class_def.fields().nth(0).unwrap().idents[0], "a");
    assert_eq!(&class_def.fields().nth(0).unwrap().ty.to_string(), "Int32");
    assert_eq!(&class_def.fields().nth(0).unwrap().idents[1], "b");
    assert_eq!(&class_def.fields().nth(0).unwrap().ty.to_string(), "Int32");
    
    assert_eq!(1, class_def.fields().nth(1).unwrap().idents.len());

    assert_eq!(&class_def.fields().nth(1).unwrap().idents[0], "c");
    assert_eq!(&class_def.fields().nth(1).unwrap().ty.to_string(), "Int32");
}

#[test]
pub fn method_decl_is_valid() {
    let unit = unit_from_string("method_decl_is_valid", r"
        implementation
        
        type MyClass = class
            function Greet: Int32;
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(1, class_def.methods().count());
    assert_eq!(0, class_def.fields().count());
    
    let method = class_def.methods().nth(0).unwrap();
    assert_eq!("Greet", method.func_decl.name.to_string());

    assert!(method.func_decl.result_ty.is_known());
    assert_eq!("Int32", class_def.methods().nth(0).unwrap().func_decl.result_ty.to_string());
}


#[test]
pub fn mixed_method_and_fields_is_valid() {
    let unit = unit_from_string("method_decl_is_valid", r"
        implementation
        
        type MyClass = class
            function Greet1;
            Value1: Int32;
            Value2: Int32;
            function Greet2;
            function Greet3;
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(3, class_def.methods().count());
    assert_eq!(2, class_def.fields().count());
}
