use terapascal_common::span::Spanned;
use crate::ast::Access;
use crate::typ::test::try_module_from_srcs;
use crate::typ::TypeError;

#[test]
fn can_construct_imported_class_with_public_fields() {
    let src_a = r"
        interface
        type
            AClass = class
            public
                a: Integer;
            end;
        implementation
        end.
    ";
    
    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := AClass(a: 123); 
        end.
    ";
    
    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];
    
    if let Err(err) = try_module_from_srcs(srcs) {
        panic!("{err}");
    }
}

#[test]
fn cannot_construct_imported_class_with_private_fields() {
    let src_a = r"
        interface
        type
            AClass = class
            private
                a: Integer;
            end;
        implementation
        end.
    ";

    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := AClass(a: 123); 
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];

    match try_module_from_srcs(srcs) {
        Ok(..) => panic!("expected access error"),
        Err(TypeError::TypeMemberInaccessible { ty, member, access, .. }) => {
            assert_eq!("UnitA.AClass", ty.to_string());
            assert_eq!("a", member.name.as_str());
            assert_eq!(Access::Private, access);
        }
        Err(other) => panic!("expected access error, got {}\n{}", other, other.span()),
    }
}
