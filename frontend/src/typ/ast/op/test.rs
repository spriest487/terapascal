use crate::ast::Access;
use crate::typ::test::{expect_type_error, try_module_from_srcs};
use crate::typ::TypeError;

#[test]
fn private_field_not_accessible_from_other_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            private
                a: Integer;
            end;
            
        function NewAClass: AClass;

        implementation
        function NewAClass: AClass;
        begin
            AClass(a: 0);
        end;

        end.
    ";

    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := NewAClass();
            var a := instance.a;
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];

    expect_type_error(try_module_from_srcs(srcs), |err| match err {
        TypeError::TypeMemberInaccessible { ty, member, access, .. } => {
            "UnitA.AClass" == ty.to_string()
                && "a" == member.name.as_str()
                && Access::Private == *access
        }
        _ => false,
    });
}

#[test]
fn public_field_accessible_from_other_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            public
                a: Integer;
            end;
            
        function NewAClass: AClass;

        implementation
        function NewAClass: AClass;
        begin
            AClass(a: 0);
        end;

        end.
    ";

    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := NewAClass();
            var a := instance.a;
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];

    try_module_from_srcs(srcs)
        .expect("should be valid");
}

#[test]
fn private_field_accessible_from_same_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            private
                a: Integer;
            end;

        initialization
            var instance := AClass(a: 0);
            var a := instance.a; 
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
    ];

    try_module_from_srcs(srcs)
        .expect("should be valid");
}
