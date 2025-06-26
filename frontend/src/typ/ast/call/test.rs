use crate::ast::Access;
use crate::typ::ast::call::overload::resolve_overload;
use crate::typ::ast::call::overload::OverloadCandidate;
use crate::typ::ast::call::test::util::candidates_from_src;
use crate::typ::ast::call::test::util::expr_from_str;
use crate::typ::ast::call::test::util::type_args_from_str;
use crate::typ::test::module_from_src;
use crate::typ::test::try_module_from_src;
use crate::typ::test::try_module_from_srcs;
use crate::typ::Invocation;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use terapascal_common::span::Span;
use terapascal_common::DiagnosticOutput;

mod util;

#[test]
fn resolves_overload_single() {
    let src = r"
        implementation
        
        function X(i: Int32);
        begin
        end;
        
        var i: Int32;

        initialization
            i := 1;
        end
    ";
    let (candidates, mut ctx) = candidates_from_src(src, "overload");

    let expr = expr_from_str("i");
    let span = expr.annotation().clone();

    let overload = resolve_overload(&candidates, &[expr], None, None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}

// explicit overloading isn't implemented!
#[test]
fn resolves_overload_by_arg_ty() {
    let src = r"
        implementation
            
        function X(i: Int32); overload;
        begin
        end;

        function X(b: Boolean); overload;
        begin
        end;
        
        var
            i: Int32 = 1;
            b: Boolean = true;
        end
    ";
    let (candidates, mut ctx) = candidates_from_src(src, "overload");

    let expr = expr_from_str("i");
    let span = expr.annotation().clone();

    let overload = resolve_overload(&candidates, &[expr], None, None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}

#[test]
fn method_call_validates_too_many_args() {
    let src = r"
        implementation
        
        type AClass = class
            function M1(a: Integer);
        end;
        
        function AClass.M1(a: Integer);
        begin
        end;
        
        initialization
            var instance := AClass();
            instance.M1(1, 2);
        end.
    ";

    match try_module_from_src("method_call_validates_too_many_args", src) {
        Err(TypeError::InvalidArgs {
            actual, expected, ..
        }) => {
            assert_eq!(3, actual.len());
            assert_eq!(2, expected.len());
        },

        Ok(..) => panic!("expected invalid args error but succeeded"),

        Err(other) => panic!("expected invalid args error, got: {}", other.main()),
    }
}

#[test]
fn method_call_validates_too_few_args() {
    let src = r"
        implementation
        
        type AClass = class
            function M1(a, b: Integer);
        end;
        
        function AClass.M1(a, b: Integer);
        begin
        end;
        
        initialization
            var instance := AClass();
            instance.M1(1);
        end.
    ";

    match try_module_from_src("method_call_validates_too_many_args", src) {
        Err(TypeError::InvalidArgs {
            actual, expected, ..
        }) => {
            assert_eq!(2, actual.len());
            assert_eq!(3, expected.len());
        },

        Ok(..) => panic!("expected invalid args error but succeeded"),

        Err(other) => panic!("expected invalid args error, got: {}", other.main()),
    }
}

#[test]
fn specializes_func_call_by_arg_ty() {
    let src = r"
        implementation

        type B = class
        end;
        
        function A[T](arg: T);
        begin
        end;
        
        initialization
            var arg := B();
            A(arg);
        end.
    ";

    let module = module_from_src("Test", src);
    let init = module.main_unit().unit.init.as_ref().unwrap();

    
    match init.body[1].annotation().as_invocation() {
        Some(Invocation::Function { function, args, .. }) => {
            assert_eq!("Test.A[Test.B]", function.name.to_string());

            assert_eq!("arg", args[0].to_string());
            assert_eq!("Test.B", args[0].annotation().ty().to_string());

            assert_eq!(
                "Test.B",
                function.name.type_args.as_ref().unwrap()[0].to_string()
            );
        },

        other => panic!("expected call to A, got {:#?}", other),
    }
}

#[test]
fn specializes_method_call_by_arg_ty() {
    let src = r"
        implementation

        type C = class
            function A[T](arg: T);
        end;
        
        type B = class
        end;
        
        function C.A[T](arg: T);
        begin
        end;
        
        initialization
            var instance := C();
            var arg := B();
            instance.A(arg);
        end.
    ";

    let module = module_from_src("Test", src);
    let init = module.main_unit().unit.init.as_ref().unwrap();

    match init.body[2].as_call() {
        Some(call) => {
            // syntactical call should have 1 arg
            assert_eq!(1, call.args.len());

            assert_eq!("instance.A", call.target.to_string());

            match call.annotation.as_invocation() {
                Some(Invocation::Method { method, type_args, args, .. }) => {
                    // call value should have 2 arg
                    assert_eq!(2, args.len());
                    assert_eq!("instance", args[0].to_string());
                    assert_eq!("Test.C", args[0].annotation().ty().to_string());

                    assert_eq!("arg", args[1].to_string());
                    assert_eq!("Test.B", args[1].annotation().ty().to_string());

                    assert_eq!("Test.C", method.self_ty.to_string());
                    assert_eq!("A", method.decl.func_decl.name.ident.to_string());

                    assert_eq!(
                        "Test.B",
                        type_args.as_ref().unwrap().items[0].to_string()
                    );
                },

                other => panic!("expected method invocation, got: {:#?}", other),
            }
        },

        other => panic!("expected call to A, got {:?}", other),
    }
}

#[test]
fn specializes_method_call_by_lambda_arg_ty() {
    let src = r"
        implementation
        
        type C = class
            function A[T](arg: function: T);
        end;
        
        type B = class
        end;
        
        function C.A[T](arg: function: T);
        begin
        end;
        
        initialization
            var instance := C();
            instance.A(lambda: B());
        end.
    ";

    let module = module_from_src("Test", src);
    let init = module.main_unit().unit.init.as_ref().unwrap();

    match init.body[1].annotation().as_invocation() {
        Some(Invocation::Method { method, args, type_args, .. }) => {
            assert_eq!("instance", args[0].to_string());
            assert_eq!("Test.C", args[0].annotation().ty().to_string());

            assert_eq!(
                "function: Test.B",
                args[1].annotation().ty().to_string()
            );

            assert_eq!("A", method.decl.func_decl.name.ident.name.as_str());
            assert_eq!("Test.C", method.self_ty.to_string());

            assert_eq!(
                "Test.B",
                type_args.as_ref().unwrap().items[0].to_string()
            );
        },

        other => panic!("expected method invocation, got: {other:?}"),
    }
}

#[test]
fn specializes_method_call_by_dynarray_element_ty() {
    let src = r"
        implementation
        
        type C = class
            function A[T](arg: array of T);
        end;
        
        type B = class
        end;
        
        function C.A[T](arg: array of T);
        begin
        end;
        
        initialization
            var instance := C();
            instance.A([] as array of B);
        end.
    ";

    let module = module_from_src("Test", src);
    let init = module.main_unit().unit.init.as_ref().unwrap();

    match init.body[1].annotation().as_invocation() {
        Some(Invocation::Method { method, args, type_args, .. }) => {
            assert_eq!(
                "array of Test.B",
                args[1].annotation().ty().to_string()
            );

            assert_eq!("instance", args[0].to_string());
            assert_eq!("Test.C", args[0].annotation().ty().to_string());

            assert_eq!("A", method.decl.func_decl.name.ident.name.as_str());
            assert_eq!("Test.C", method.self_ty.to_string());

            assert_eq!(
                "Test.B",
                type_args.as_ref().unwrap().items[0].to_string()
            );
        }

        other => panic!("expected method invocation, got: {other:?}"),
    }
}

#[test]
fn specializes_free_func_call_by_dynarray_element_ty() {
    let src = r"
        implementation
        uses System;
        
        function A[T](arg: array of T);
        begin
        end;
        
        type B = class
        end;
        
        initialization
            var arr: array of B := [];
            arr.A;
        end.
    ";

    let module = module_from_src("Test", src);
    let init = module.main_unit().unit.init.as_ref().unwrap();

    match init.body[1].annotation().as_invocation() {
        Some(Invocation::Function {
            function,
            args,
            type_args,
            ..
        }) => {
            assert_eq!("array of Test.B", args[0].annotation().ty().to_string());

            assert_eq!("Test.A[Test.B]", function.name.to_string());
            assert_eq!("Test.B", type_args.as_ref().unwrap().items[0].to_string());
        },

        Some(other) => {
            panic!("expected invocation of A, got {:?}", other)
        },

        None => {
            panic!(
                "expected invocation of A, got: {:?}",
                init.body[1].annotation()
            )
        },
    }
}

#[test]
fn overload_with_accessible_method_is_ambiguous() {
    let a_src = r"
        interface
        type MyClass = class
        public
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my: MyClass);
        
        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass;
            i.A();
        end
    ";

    let result = try_module_from_srcs([("UnitA", a_src), ("UnitB", b_src)]);

    match result {
        Ok(..) => panic!("call to A should be ambiguous"),
        Err(TypeError::AmbiguousFunction { candidates, .. }) => {
            candidates
                .iter()
                .find(|candidate| match candidate {
                    OverloadCandidate::Method { iface_ty, decl, .. } => {
                        iface_ty.to_string() == "UnitA.MyClass"
                            && decl.func_decl.ident().as_str() == "A"
                    },
                    _ => false,
                })
                .expect("must have a candidate for the public method");

            candidates
                .iter()
                .find(|candidate| match candidate {
                    OverloadCandidate::Function { decl_name, .. } => {
                        decl_name.to_string() == "UnitA.A"
                    },
                    _ => false,
                })
                .expect("must have a candidate for the free function");
        },

        Err(other) => panic!("expected ambiguous function error, got: {}", other),
    }
}

/// if a method and a free function both match a call, but the method is inaccessible from the
/// call's context, it should resolve to the function instead of being ambiguous
#[test]
fn overload_with_inaccessible_method_is_not_ambiguous() {
    let a_src = r"
        interface
        type MyClass = class 
        private
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my: MyClass);

        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass();
            i.A();
        end
    ";

    let result = try_module_from_srcs([("UnitA", a_src), ("UnitB", b_src)]);

    result.expect("call to A should not be ambiguous");
}

/// if an inaccessible method and a function have the same name, but only the method matches,
/// it should resolve the method and report an access error instead of being ambiguous
#[test]
fn overload_resolves_inaccessible_method_if_only_match() {
    let a_src = r"
        interface
        type MyClass = class 
        private
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my, extra: MyClass);

        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my, extra: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass();
            i.A();
        end
    ";

    let result = try_module_from_srcs([("UnitA", a_src), ("UnitB", b_src)]);

    match result {
        Ok(..) => panic!("call to A should be inaccessible"),

        Err(TypeError::TypeMemberInaccessible { member, access, .. }) => {
            assert_eq!("A", member.name.as_str());
            assert_eq!(Access::Private, access);
        },

        Err(other) => panic!("expected access error, got: {}", other),
    }
}

static OVERLOAD_GENERIC_UNIT: &str = r"
    implementation
        
    function X(a, b: Int32): Int32; overload;
    begin
        a
    end;

    function X[T](t: T): T; overload;
    begin
        t
    end;
    
    end
";

#[test]
fn resolving_overload_with_generic_discarded() {
    let (candidates, mut ctx) = candidates_from_src(OVERLOAD_GENERIC_UNIT, "overload");
    let span = Span::zero("overload");

    let overload = match resolve_overload(
        &candidates,
        &[expr_from_str("100"), expr_from_str("200")],
        None,
        None,
        &span,
        &mut ctx,
    ) {
        Ok(result) => result,
        Err(err) => panic!("{:#?}", err),
    };

    assert_eq!(overload.type_args, None);

    let selected = &candidates[overload.selected_sig];
    assert_eq!(selected.decl().result_ty, Type::Primitive(Primitive::Int32));
    assert_eq!(selected.decl().params_len(), 2);
    assert!(selected.decl().name.type_params.is_none());
}

#[test]
fn resolving_overload_with_generic_selected() {
    let (candidates, mut ctx) = candidates_from_src(OVERLOAD_GENERIC_UNIT, "overload");
    let span = Span::zero("overload");

    let overload = match resolve_overload(
        &candidates,
        &[expr_from_str("'Test'")],
        Some(&type_args_from_str(&["String"], &mut ctx)),
        None,
        &span,
        &mut ctx,
    ) {
        Ok(result) => result,
        Err(err) => panic!("{:#?}", err),
    };

    assert_eq!(
        overload.type_args.unwrap().items[0].to_string(),
        "System.String"
    );
}
