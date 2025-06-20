use super::*;
use crate::ast::expression::test::parse_expr;
use crate::ast::TypeNamePatternKind;

fn parse_if_cond(src: &str) -> IfCond<Expr<Span>> {
    match parse_expr(src) {
        Expr::IfCond(if_cond) => *if_cond,
        expr => panic!("expected expr to be an if condition, got: {:?}", expr),
    }
}

#[test]
fn parses_without_is_pattern() {
    let cond = parse_if_cond("if x then y");
    assert!(cond.is_pattern.is_none());
}

#[test]
fn parses_with_is_pattern() {
    let cond = parse_if_cond("if x is String then y");
    assert!(cond.is_pattern.is_some());

    match cond.is_pattern.as_ref().unwrap() {
        TypeNamePattern::ExactType { name, kind, .. } => {
            assert_eq!(name.to_string(), "String");
            assert_eq!(*kind, TypeNamePatternKind::Is);
        },

        _ => panic!("expected positive binding"),
    }
}

#[test]
fn parses_with_is_not_pattern() {
    let cond = parse_if_cond("if x is not String then y");
    assert!(cond.is_pattern.is_some());

    match cond.is_pattern.as_ref().unwrap() {
        TypeNamePattern::ExactType { name, .. } => {
            assert_eq!("String", name.to_string());
        },

        _ => panic!("expected negative binding"),
    }
}

#[test]
fn parses_with_is_pattern_and_binding() {
    let cond = parse_if_cond("if x is String s then y");
    assert!(cond.is_pattern.is_some());

    match cond.is_pattern.as_ref().unwrap() {
        TypeNamePattern::ExactType {
            name,
            kind: TypeNamePatternKind::IsWithBinding { binding },
            ..
        } => {
            assert_eq!(name.to_string(), "String");
            assert_eq!(binding.to_string(), "s".to_string());
        },

        _ => panic!("expected positive binding"),
    }
}
