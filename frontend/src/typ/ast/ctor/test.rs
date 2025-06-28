use crate::ast::Access;
use crate::typ::test::print_errors;
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

    let srcs = [("UnitA", src_a), ("UnitB", src_b)];

    if let Err(errors) = try_module_from_srcs(srcs) {
        print_errors(&errors);
        panic!("expected success");
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

    let srcs = [("UnitA", src_a), ("UnitB", src_b)];

    let errors = try_module_from_srcs(srcs).expect_err("expected access error");

    let (ty, member, access) = errors
        .iter()
        .find_map(|err| match err {
            TypeError::TypeMemberInaccessible {
                ty, member, access, ..
            } => Some((ty, member, *access)),
            _ => None,
        })
        .unwrap_or_else(|| {
            print_errors(&errors);
            panic!("expected access error")
        });

    assert_eq!("UnitA.AClass", ty.to_string());
    assert_eq!("a", member.name.as_str());
    assert_eq!(Access::Private, access);
}
