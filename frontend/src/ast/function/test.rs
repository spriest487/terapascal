use super::*;
use crate::ast::IdentPath;
use crate::pp::Preprocessor;
use terapascal_common::aggregate_err::AggregateError;
use terapascal_common::fs::DefaultFilesystem;
use terapascal_common::CompileOpts;

fn try_parse_func_decl(src: &str) -> ParseResult<FunctionDecl> {
    let test_unit = Preprocessor::new(&DefaultFilesystem, "test", CompileOpts::default())
        .preprocess(src)
        .unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();
    let token_stream = TokenStream::new(tokens, Span::zero("test"));
    let mut parser = Parser::new(token_stream);

    let decl = FunctionDecl::parse(&mut parser, false, Vec::new())?;
    
    let errors = parser.finish();

    AggregateError::result(decl, errors)
        .map_err(|err| *err.first)
}

fn parse_func_decl(src: &str) -> FunctionDecl<Span> {
    try_parse_func_decl(src).unwrap()
}

fn make_ident_path<const N: usize>(names: [&str; N]) -> IdentPath {
    IdentPath::from_parts(names.iter()
        .map(|n| Ident::new(n, Span::zero("test")))
    )
}

fn func_iface(decl: &FunctionDecl) -> Option<TypePath> {
    decl.name.owning_ty_qual.clone().map(|boxed_ty| *boxed_ty)
}

fn make_iface_ty<const N: usize>(name: IdentPath, ty_param_names: [&str; N]) -> TypePath {
    let span = Span::zero("test");
    
    let type_params = if ty_param_names.len() > 0 {
        Some(TypeList::new(ty_param_names
            .into_iter()
            .map(|name| Ident::new(name, span.clone())),
            span.clone()
        ))
    } else {
        None
    };
    
    TypePath {
        name,
        name_span: span.clone(),
        type_params,
        span,
    }
}

#[test]
fn parses_simple_name_decl() {
    let decl = parse_func_decl("function TestFunc()");

    assert_eq!("TestFunc", decl.name.to_string());
}

#[test]
fn parses_unqualified_iface_impl_decl() {
    let decl = parse_func_decl("function I.TestFunc(self: A)");

    assert_eq!(
        Some(make_iface_ty(make_ident_path(["I"]), [])),
        func_iface(&decl)
    );
    assert_eq!("TestFunc", decl.name.ident.to_string());
    assert_eq!("I.TestFunc", decl.name.to_string());
}

#[test]
fn parses_qualified_iface_impl_decl() {
    let decl = parse_func_decl("function NS_A.I.TestFunc(self: A)");

    assert_eq!(
        Some(make_iface_ty(make_ident_path(["NS_A", "I"]), [])),
        func_iface(&decl)
    );
    assert_eq!("TestFunc", decl.name.ident.to_string());
    assert_eq!("NS_A.I.TestFunc", decl.name.to_string());
}

#[test]
fn parses_func_with_type_params() {
    let decl = parse_func_decl("function TestFunc[A]()");

    assert_eq!("TestFunc", decl.name.ident.to_string());
    assert_eq!("TestFunc[A]", decl.name.to_string());

    let decl_ty_params = decl.name.type_params.expect("expected type params to be parsed");
    assert_eq!(1, decl_ty_params.items.len());
    assert_eq!("A", decl_ty_params.items[0].name.to_string());
}

#[test]
fn parses_method_of_interface_with_type_args() {
    let decl = parse_func_decl("function A[B].TestFunc()");

    assert_eq!("TestFunc", decl.name.ident.to_string());

    let expect_iface = make_iface_ty(make_ident_path(["A"]), ["B"]);
    assert_eq!(Some(expect_iface), func_iface(&decl));
}

#[test]
fn parses_method_of_interface_with_type_args_with_type_params() {
    let decl = parse_func_decl("function A[B].TestFunc[C]()");

    let impl_ty = func_iface(&decl);
    let expect_iface = make_iface_ty(make_ident_path(["A"]), ["B"]);
    assert_eq!(Some(expect_iface), impl_ty);

    assert_eq!("TestFunc", decl.name.ident.to_string());

    let decl_ty_params = decl.name.type_params.expect("expected type params to be found");
    assert_eq!(1, decl_ty_params.items.len());
    assert_eq!("C", decl_ty_params.items[0].name.to_string());
}

#[test]
fn fails_with_invalid_type_params() {
    let result = try_parse_func_decl("function A[B].TestFunc[^Integer]()");
    
    assert!(result.is_err());

    let err = result.unwrap_err().err;
    let is_error_on_int_ptr = match &err {
        ParseError::UnexpectedToken(tt, ..) => tt.is_operator(Operator::Caret),
        _ => false,
    };
    
    assert!(is_error_on_int_ptr, "was: {:#?}", err);
}

#[test]
fn rejects_tokens_after_param_list() {
    let result = try_parse_func_decl("function A(a: Integer foo)");
    let err = result.err().expect("should return a parse error");

    let is_unexpected_foo = match &err.err {
        ParseError::UnexpectedToken(tt, ..) => tt.is_ident("foo"),
        _ => false,
    };
    
    assert!(is_unexpected_foo, "was: {:#?}", err.err)
}
