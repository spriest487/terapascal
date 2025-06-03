use crate::ast::{Expr, ObjectCtor};
use crate::TokenStream;
use terapascal_common::span::Span;
use terapascal_common::CompileOpts;

fn expr_from_src(src: &str) -> Expr {
    let opts = CompileOpts::default();
    let unit = crate::preprocess("test", src, opts).unwrap();
    let tokens = crate::tokenize(unit).unwrap();
    let span = Span::zero("test");

    let mut token_stream = TokenStream::new(tokens, span);

    match Expr::parse(&mut token_stream) {
        Ok(expr) => expr,
        Err(err) => panic!("{}", err.err),
    }
}

fn to_ctor(expr: Expr) -> ObjectCtor {
    match expr {
        Expr::ObjectCtor(ctor) => *ctor,
        Expr::Call(call) => call.try_into_func_call()
            .expect("expected to parse as a function call")
            .try_into_empty_object_ctor()
            .expect("function call must be convertible to a ctor"),
        invalid => panic!("parsed invalid non-ctor expression: {invalid:?}"),
    }
} 

#[test]
fn empty_ctor_valid() {
    let ctor = to_ctor(expr_from_src("A()"));
    assert_eq!("A", ctor.type_expr.unwrap().to_string());
    assert_eq!(0, ctor.args.members.len());
}

#[test]
#[should_panic]
fn partial_argument_invalid() {
    expr_from_src("A(i: )");
}

#[test]
fn single_arg_valid() {
    let ctor = to_ctor(expr_from_src("A(a: 1)"));
    assert_eq!("A", ctor.type_expr.unwrap().to_string());
    assert_eq!(1, ctor.args.members.len());

    assert_eq!("a", ctor.args.members[0].ident.name.as_str());
    assert_eq!("1", ctor.args.members[0].value.to_string());
}

#[test]
fn multi_arg_valid() {
    let ctor = to_ctor(expr_from_src("A(a: 1; b: 'test')"));
    assert_eq!("A", ctor.type_expr.unwrap().to_string());
    assert_eq!(2, ctor.args.members.len());
    
    assert_eq!("a", ctor.args.members[0].ident.name.as_str());
    assert_eq!("1", ctor.args.members[0].value.to_string());

    assert_eq!("b", ctor.args.members[1].ident.name.as_str());
    assert_eq!("'test'", ctor.args.members[1].value.to_string());
}

#[test]
#[should_panic]
fn trailing_comma_invalid() {
    to_ctor(expr_from_src("A.B(a: 1,)"));
}

#[test]
fn ty_args_valid() {
    let ctor = to_ctor(expr_from_src("A[B, C]()"));
    assert_eq!("A", ctor.type_expr.unwrap().to_string());
    
    let ty_args = ctor.type_args.expect("no type args were parsed");
    
    assert_eq!(2, ty_args.len());
    assert_eq!("B", ty_args[0].to_string());
    assert_eq!("C", ty_args[1].to_string());
}
