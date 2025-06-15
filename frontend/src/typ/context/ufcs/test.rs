use crate::ast;
use crate::typ::context::ufcs::find_ufcs_free_functions;
use crate::typ::test::try_unit_from_src;
use crate::typ::test::unit_from_src;
use crate::typ::test::units_from_src;
use crate::typ::{InstanceMethod, InvocationValue, NameError, TypeError, Value};
use crate::typ::Type;
use crate::typ::SYSTEM_UNIT_NAME;

#[test]
fn finds_ufcs_func() {
    let unit = unit_from_src(
        "test",
        r"implementation

        type UFCSTarget = class
        end;

        function TargetMethod(t: UFCSTarget);
        begin
        end;
        
        end",
    );

    let (_, target_decl) = unit.unit.type_decl_items().next().unwrap();
    let target = Type::of_decl(target_decl, &unit.context).unwrap();
    assert_eq!(target.full_path().unwrap().last().name.as_str(), "UFCSTarget");

    let methods = ignore_system_funcs(find_ufcs_free_functions(&target, &unit.context));

    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
}

#[test]
fn finds_exported_ufcs_func_from_other_unit() {
    let a_src = r"
        interface

        type UFCSTarget = class
        end;

        end";

    let b_src = r"
        interface

        function TargetMethod(t: A.UFCSTarget);
        
        implementation

        function TargetMethod(t: A.UFCSTarget);
        begin
        end;

        end";

    let c_src = r"
        implementation
        uses A;
        uses B;
        end";

    let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

    let a = &units["A"];
    let c = &units["C"];

    let (_, target_decl) = a.unit.type_decl_items().next().unwrap();
    let target = Type::of_decl(target_decl, &a.context).unwrap();
    let methods = ignore_system_funcs(find_ufcs_free_functions(&target, &c.context));

    assert_eq!(methods.len(), 1);
    assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
}

#[test]
fn doesnt_find_private_ufcs_func_from_other_unit() {
    let a_src = r"
        implementation
        
        type UFCSTarget = class
        end;
        
        end.";

    let b_src = r"
        implementation
        function TargetMethod(t: A.UFCSTarget);
        begin
        end;

        end.";

    let c_src = r"
        implementation
        uses A;
        uses B;
        end.";

    let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

    let a = &units["A"];
    let c = &units["C"];

    let target = Type::of_decl(&a.unit.type_decl_items().next().unwrap().1, &c.context).unwrap();
    let methods = ignore_system_funcs(find_ufcs_free_functions(&target, &c.context));

    assert_eq!(ignore_system_funcs(methods).len(), 0);
}

#[test]
fn generic_class_method_has_correct_self_ty() {
    let src = r"
        implementation
        
        type MyClass[T] = class
            val: Integer;
            function MyMethod;
        end;
        
        function MyClass[T].MyMethod;
        begin
        end;

        initialization
            var b := MyClass[Integer](val: 1);
            b.MyMethod();
        end.
    ";
    
    // eprintln!("{:#?}", {
    //     let pp_unit = crate::preprocess("test", src, crate::BuildOptions::default())
    //         .unwrap();
    //     let tokens = crate::tokenize(pp_unit).unwrap();
    //     let parsed = crate::parse("test", tokens).unwrap();
    //     parsed
    // });
    
    unit_from_src("test", src);
}

#[test]
fn doesnt_find_free_func_with_mismatched_type_constraint() {
    let src = r"
        implementation

        type
            IMyConstraint = interface
            end;
            
            MyClass = class of IMyConstraint
            end;

        function MyFunc[T](t: T)
        where 
            T is IMyConstraint;
        begin
        end;

        initialization
            var x: Integer := 123;
            x.MyFunc;
        end.
    ";
    
    match try_unit_from_src("Test", src) {
        Ok(..) => panic!("expected error"),
        
        Err(TypeError::NameError { err: NameError::MemberNotFound { member, .. }, .. }) => {
            assert_eq!(member.to_string(), "MyFunc")
        }
        
        Err(other) => {
            panic!("expected member not found error, got {}", other)
        },
    }
}

#[test]
fn finds_free_func_with_matched_type_constraint() {
    let src = r"
        implementation
        
        type
            IMyConstraint = interface
            end;
            
            MyClass = class of IMyConstraint
            end;
                
        function MyFunc[T](t: T) 
        where 
            T is IMyConstraint;
        begin
        end;

        initialization
            var x := MyClass();
            x.MyFunc;
        end.
    ";

    if let Err(other) = try_unit_from_src("Test", src) {
        panic!("expected success, got error:\n{}", other)
    }
}

#[test]
fn finds_func_in_its_own_body() {
    let unit = unit_from_src("Test", r"
        implementation
        
        type 
            A = class 
            end;

        function MyFunc(a: A);
        begin
            a.MyFunc;
        end;
        end.
    ");

    let ast::UnitDecl::FunctionDef { def } = &unit.unit.impl_decls[1] else {
        panic!("expected second item to be a function");
    };
    
    let Value::Invocation(invocation) = def.body.stmts[0].annotation() else {
        panic!("expected first statement to be an invocation");
    };

    match invocation.as_ref() {
        InvocationValue::Function { function, args, .. } => {
            assert_eq!("Test.MyFunc", function.name.to_string());
            assert_eq!("a", args[0].to_string());
        }
        
        _ => {
            panic!("expected function invocation");
        }
    }
}

// all these tests should ignore any builtin UFCS candidates, we're only checking for the
// ones defined in the test units
fn ignore_system_funcs(mut methods: Vec<InstanceMethod>) -> Vec<InstanceMethod> {
    methods.retain(|im| {
        match im {
            InstanceMethod::Method { .. } => true,
            InstanceMethod::FreeFunction { func_name, .. } => {
                func_name.full_path.as_slice()[0].name.as_str() != SYSTEM_UNIT_NAME
            },
        }
    });
    methods
}
