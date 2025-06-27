use crate::ast;
use super::*;
use crate::ast::expression::test::parse_expr;

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

    match &cond.is_pattern.as_ref().unwrap().pattern {
        MatchPattern::Name { name, .. } => {
            assert_eq!("String", name.to_string());
        }

        _ => panic!("expected match on String"),
    }
}

#[test]
fn parses_with_is_not_pattern() {
    let cond = parse_if_cond("if x is not String then y");
    assert!(cond.is_pattern.is_some());

    match &cond.is_pattern.as_ref().unwrap().pattern {
        ast::MatchPattern::Not { pattern, .. } => {
            match pattern.as_ref() {
                ast::MatchPattern::Name { name, .. } => {
                    assert_eq!("String", name.to_string());        
                }
                
                _ => panic!("expected inner pattern `String`"),
            }
            
        }

        _ => panic!("expected negative match on String"),
    }
}

#[test]
fn parses_with_is_pattern_and_binding() {
    let cond = parse_if_cond("if x is String myStr then y");
    assert!(cond.is_pattern.is_some());

    match &cond.is_pattern.as_ref().unwrap().pattern {
        MatchPattern::Name { name, .. } => {
            assert_eq!("String", name.to_string());
        }

        _ => panic!("expected match on String"),
    }
    
    assert_eq!("myStr", cond.is_pattern.unwrap().binding.unwrap().name.as_str());
}
