use crate::ast::util::tokens_from_string;
use crate::ast::DeclIdent;
use crate::ast::Ident;
use crate::ast::InterfaceDecl;
use crate::parse::{ContinueParse, Parser};
use crate::parse::ParseError;
use crate::Keyword;
use crate::TokenTree;
use terapascal_common::span::Span;
use terapascal_common::TracedError;

fn iface_decl_from_src(src: &str) -> Result<InterfaceDecl, Vec<TracedError<ParseError>>> {
    let tokens = tokens_from_string("Test", src);
    let mut parser = Parser::new(tokens);

    let name = DeclIdent {
        ident: Ident::new("ITest", Span::zero("Test")),
        span: Span::zero("Test"),
        type_params: None,
    };

    let kw = TokenTree::Keyword {
        kw: Keyword::Interface,
        span: Span::zero("Test"),
    };

    let decl = InterfaceDecl::parse(&mut parser, name, vec![], kw)
        .ok_or_continue(parser.errors());
    let errors = parser.finish();
    
    match decl {
        Some(decl) if errors.is_empty() => Ok(decl), 
        _ => Err(errors)
    }
}

#[test]
pub fn parses_with_empty_members() {
    let iface = iface_decl_from_src(
        r"
        interface
        end
        ",
    ).unwrap();
    
    assert_eq!(0, iface.methods.len())
}

#[test]
pub fn parses_with_single_separated_member() {
    let iface = iface_decl_from_src(
        r"
        interface
        function A: T;
        end
        ",
    ).unwrap();

    assert_eq!(1, iface.methods.len())
}

#[test]
pub fn parses_with_single_unseparated_member() {
    let iface = iface_decl_from_src(
        r"
        interface
        function A: T
        end
        ",
    ).unwrap();

    assert_eq!(1, iface.methods.len())
}

#[test]
pub fn parses_without_final_separator() {
    let iface = iface_decl_from_src(
        r"
        interface
        function A: T;
        function B: T
        end
        ",
    ).unwrap();

    assert_eq!(2, iface.methods.len())
}

#[test]
pub fn parses_with_final_separator() {
    let iface = iface_decl_from_src(
        r"
        interface
        function A: T;
        function B: T;
        end
        ",
    ).unwrap();

    assert_eq!(2, iface.methods.len())
}
