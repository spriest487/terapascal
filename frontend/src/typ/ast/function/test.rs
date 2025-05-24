use crate::ast::FunctionDeclKind;
use crate::ast::TypeConstraint;
use crate::ast::TypeParam;
use crate::typ::ast::{specialize_func_decl, WhereClause};
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionParam;
use crate::typ::ast::TypedFunctionName;
use crate::typ::builtin_displayable_name;
use crate::typ::test::module_from_src;
use crate::typ::test::try_module_from_src;
use crate::typ::test::try_module_from_srcs;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::InvalidOverloadKind;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeParamList;
use crate::typ::TypeParamType;
use crate::typ::TypeResult;
use crate::Ident;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

fn test_span() -> Span {
    Span::zero("test")
}

fn test_ident(name: &str) -> Ident {
    Ident::new(name, test_span())
}

fn make_ty_param(name: &str) -> TypeParam<Type> {
    TypeParam {
        name: test_ident(name),
        constraint: None,
    }
}

fn make_ty_param_of(name: &str, constraint: Type) -> TypeParam<Type> {
    let name = test_ident(name);
    
    TypeParam {
        name: name.clone(),
        constraint: Some(TypeConstraint {
            span: name.span.clone(),
            name: name,
            is_ty: constraint,
        }),
    }
}

fn make_ty_param_ty(param_list: &[TypeParam<Type>], pos: usize) -> Type {
    let param = &param_list[pos];

    Type::GenericParam(Rc::new(TypeParamType {
        name: param.name.clone(),
        is_ty: param
            .constraint
            .as_ref()
            .map(|constraint| constraint.is_ty.clone())
            .unwrap_or(Type::Any),
    }))
}

fn make_decl(
    owning_ty: Option<Type>,
    name: &str,
    ty_params: impl IntoIterator<Item=TypeParam<Type>>,
    where_clause: Option<WhereClause>,
    params: impl IntoIterator<Item=Type>,
    return_ty: Type
) -> FunctionDecl {
    let test_span = test_span();
    
    let ty_params: Vec<_> = ty_params.into_iter().collect();
    let ty_params_list = if ty_params.len() == 0 {
        None
    } else {
        Some(TypeParamList::new(ty_params, test_span.clone()))
    };

    FunctionDecl {
        name: TypedFunctionName{
            ident: test_ident(name),
            owning_ty,
            span: test_span.clone(),
            type_params: ty_params_list,
        },
        kind: FunctionDeclKind::Function,
        params: params
            .into_iter()
            .enumerate()
            .map(|(pos, ty)| FunctionParam {
                ident: test_ident(&format!("arg{}", pos)),
                ty,
                modifier: None,
                span: test_span.clone(),
            })
            .collect(),
        return_ty,
        mods: Vec::new(),
        span: test_span.clone(),
        where_clause,
    }
}

#[test]
fn specialized_func_decl_has_specialized_return_ty() {
    let ty_params = [make_ty_param("T")];
    let return_ty = make_ty_param_ty(&ty_params, 0);

    let decl = make_decl(None, "A", ty_params, None, [], return_ty);
    let ctx = Context::root(test_span());
    
    let args = TypeArgList::new([Primitive::Int32.into()], test_span());

    let specialized = specialize_func_decl(&decl, &args, &ctx)
        .unwrap();
    
    assert_eq!(Type::Primitive(Primitive::Int32), specialized.return_ty);
}

#[test]
fn specialized_func_decl_has_specialized_param_tys() {
    let ty_params = [make_ty_param("T0"), make_ty_param("T1")];

    let arg0_ty = make_ty_param_ty(&ty_params, 1);
    let arg1_ty = make_ty_param_ty(&ty_params, 0);

    let decl = make_decl(None, "A", ty_params, None, [arg0_ty, arg1_ty], Type::Nothing);
    let ctx = Context::root(test_span());

    let args = TypeArgList::new([
        Primitive::Int32.into(),
        Primitive::Boolean.into(),
    ], test_span());

    let specialized = specialize_func_decl(&decl, &args, &ctx)
        .unwrap();

    assert_eq!(Type::Primitive(Primitive::Boolean), specialized.params[0].ty);
    assert_eq!(Type::Primitive(Primitive::Int32), specialized.params[1].ty);
}

#[test]
fn specialized_func_decl_checks_constraint() {
    let displayable = Type::interface(builtin_displayable_name().full_path);
    
    let ty_params = [make_ty_param_of("T", displayable)];

    let decl = make_decl(None, "A", ty_params, None, [], Type::Nothing);
    
    let ctx = Context::root(test_span());
    
    // Any should implement no interfaces
    let args = TypeArgList::new([Type::Any], test_span());

    match specialize_func_decl(&decl, &args, &ctx) {
        Err(GenericError::ConstraintNotSatisfied { .. }) => { 
            // ok
        },
        Err(other) => panic!("expected constraint violation error, but got {other}"),
        Ok(..) => panic!("expected constraint violation error, but succeeded"),
    }
}

#[test]
fn duplicate_overload_is_error() {
    let result = try_module_from_src("Test", r"
    interface

    function A(x: Boolean); overload;
    function A(x: Integer); overload;
    function A(x: Integer); overload;
    
    end.
    ");
    
    match result {
        Err(TypeError::InvalidFunctionOverload { kind, ident, .. }) => {
            assert_eq!("A", ident.name.as_str());
            assert_eq!(
                InvalidOverloadKind::Duplicate(1), 
                kind
            );
        }

        Ok(..) => panic!("expected an error"),
        Err(other) => panic!("expected invalid overload error, got:\n{}\n{}", other, other.span()),
    }
}

#[test]
fn missing_overload_modifier_on_existing_is_error() {
    let result = try_module_from_src("Test", r"
    interface

    function A(x: Boolean);
    function A(x: Integer); overload;
    
    end.
    ");

    expect_missing_overload(result);
}

#[test]
fn missing_overload_modifier_on_new_is_error() {
    let result = try_module_from_src("Test", r"
    interface
    
    function A(x: Boolean); overload;
    function A(x: Integer);
    
    end.
    ");

    expect_missing_overload(result);
}

fn expect_missing_overload<T>(result: TypeResult<T>) {
    match result {
        Err(TypeError::InvalidFunctionOverload { kind, ident, .. }) => {
            assert_eq!("A", ident.name.as_str());
            assert_eq!(
                InvalidOverloadKind::MissingOverloadModifier,
                kind
            );
        }

        Err(other) => panic!("expected invalid overload error, got:\n{}", other),
        
        Ok(..) => panic!("expected an error"),
    }
}

#[test]
fn mixed_visibility_overload_is_ok() {
    module_from_src("Test", r"
        interface
        
        function A(x: Boolean); overload;
        
        implementation
        
        function A(x: Boolean); overload;
        begin
        end;
    
        function A(x: Integer); overload;
        begin
        end;
        
        end.
    ");
}

#[test]
fn calling_invisible_function_from_outside_unit_is_err() {
    let srcs = [
        ("UnitA", r"
            interface
            
            implementation
        
            function A(x: Integer); overload;
            begin
            end;
            
            end.
        "),
        ("UnitB", r"
            implementation
            uses UnitA;
            
            initialization
                A(123);
            end.
        "),
    ];

    expect_not_visible_err(try_module_from_srcs(srcs));
}

#[test]
fn calling_less_visible_overload_from_outside_unit_is_err() {
    let srcs = [
        ("UnitA", r"
            interface
            
            function A(x: Boolean); overload;
            
            implementation
            
            function A(x: Boolean); overload;
            begin
            end;
        
            function A(x: Integer); overload;
            begin
            end;
            
            end.
        "),
        ("UnitB", r"
            implementation
            uses UnitA;
            
            initialization
                A(123);
            end.
        "),
    ];
    
    expect_not_visible_err(try_module_from_srcs(srcs));
}

fn expect_not_visible_err<T>(result: TypeResult<T>) {
    match result {
        Err(TypeError::NameNotVisible { name, .. }) => {
            assert_eq!("UnitA.A", name.to_string());
        }

        Err(other) => panic!("expected name not visible error, got: {}", other),

        Ok(..) => panic!("expected name not visible error, got success"),
    }
}
