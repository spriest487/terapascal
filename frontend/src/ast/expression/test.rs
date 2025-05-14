use super::*;
use crate::ast::type_name::IdentTypeName;
use crate::ast::type_name::TypeName;
use crate::pp::Preprocessor;
use crate::token_tree::TokenTree;
use crate::Operator;
use common::BuildOptions;
use common::TracedError;

fn tokenize(src: &str) -> TokenStream {
    let test_unit = Preprocessor::new("test", BuildOptions::default())
        .preprocess(src)
        .unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();

    TokenStream::new(tokens, Span::zero("test"))
}

fn try_parse_expr(src: &str) -> ParseResult<Expr<Span>> {
    let mut tokens = tokenize(src);
    Expr::parse(&mut tokens).and_then(|expr| {
        tokens.finish()?;
        Ok(expr)
    })
}

pub fn parse_expr(src: &str) -> Expr<Span> {
    try_parse_expr(src).unwrap()
}

/// for an expr like x := Y() we should read it as the statement x := (Y()) 
/// not the invalid binary operation (x := Y)()
#[test]
fn parse_assignment_then_call_in_order_as_stmt() {
    let assignment = match try_parse_expr("x := Y()") {
        Err(TracedError { err: ParseError::StatementIsIllegal(stmt), .. }) => match *stmt {
            Stmt::Assignment(assignment) => *assignment,
            other => panic!("expected assignment, got {}", other),
        }
        
        other => panic!("expected statement, got {:?}", other),
    };
    
    let lhs_ident = assignment.lhs.as_ident().unwrap_or_else(|| {
        panic!(
            "parsed statement should have ident x on the lhs, found {}",
            assignment.lhs
        )
    });

    assert_eq!("x", lhs_ident.name.as_str());
}

#[test]
fn parse_assignment_then_call_in_order_explicit() {
    let assignment = match try_parse_expr("x := (Y())") {
        Err(TracedError { err: ParseError::StatementIsIllegal(stmt), .. }) => match *stmt {
            Stmt::Assignment(assignment) => *assignment,
            other => panic!("expected assignment, got {}", other),
        }

        other => panic!("expected statement, got {:?}", other),
    };

    let lhs_ident = assignment.lhs.as_ident().unwrap_or_else(|| {
        panic!(
            "parsed statement should have ident x on the lhs, found {}",
            assignment.lhs
        )
    });

    assert_eq!("x", lhs_ident.name.as_str());
}

#[test]
fn invocation_with_empty_type_args_is_error() {
    let input = "A[]()";
    match try_parse_expr(input) {
        Ok(invalid) => panic!("expected error, got expr {:#?}", invalid),
        Err(TracedError { err: ParseError::EmptyTypeArgList(..), .. }) => {},
        Err(invalid) => panic!("unexpected error: {}", invalid.err),
    }
}

#[test]
fn parse_invocation_with_type_args() {
    match parse_expr("A[B, C]()") {
        Expr::Call(call) => match call.as_ref() {
            Call::Function(func_call) => {
                let target_ident = func_call.target.as_ident().unwrap_or_else(|| {
                    panic!("parsed func call should have ident as the call target");
                });
                assert_eq!(target_ident.name.as_str(), "A");

                let type_args = func_call.type_args.as_ref().unwrap_or_else(|| {
                    panic!("parsed func call should have type args")
                });

                let expect_args = [ "B", "C" ];
                assert_eq!(expect_args.len(), type_args.items.len());

                for (&expected, actual) in expect_args.iter().zip(type_args.items.iter()) {
                    match actual {
                        TypeName::Ident(IdentTypeName { ident, .. }) => {
                            assert_eq!(None, ident.parent());
                            assert_eq!(expected, ident.last().name.as_str());
                        }

                        invalid => panic!("expected all args to be idents, got {:#?}", invalid)
                    }
                }
            },
            invalid => panic!("expected function call, got {:#?}", invalid)
        },
        invalid => panic!("expected call, got {:#?}", invalid)
    }
}

#[test]
fn member_indexer_parse_order() {
    let expr = parse_expr("x.y[1]");

    match expr.as_bin_op() {
        Some(BinOp { op: Operator::Index, lhs: base, rhs: index, .. }) => {
            match base {
                Expr::BinOp(bin_op) => {
                    let lhs_ident = bin_op.lhs.as_ident().expect("lhs should be an ident");
                    let rhs_ident = bin_op.rhs.as_ident().expect("rhs should be an ident");

                    assert_eq!(lhs_ident.name.as_str(), "x");
                    assert_eq!(rhs_ident.name.as_str(), "y");
                }
                invalid => panic!("expected indexer base expr, got {:#?}", invalid),
            }

            match index {
                Expr::Literal(Literal::Integer(int_const), ..) => {
                    assert_eq!(1, int_const.as_i32().unwrap());
                }
                invalid => panic!("expected indexer index expr, got {:#?}", invalid),
            }
        }

        _ => panic!("expected indexer expr, got {:#?}", expr)
    }
}

#[test]
fn addr_of_indexer_parse_order() {
    let expr = parse_expr("@x[1]");

    let unary_op = match expr.as_unary_op() {
        Some(o) => o,
        None => panic!("expr must parse as a unary op, got: {:#?}", expr),
    };

    assert_eq!(Operator::AddressOf, unary_op.op);

    let operand_bin_op = match unary_op.operand.as_bin_op() {
        Some(o) => o,
        None => panic!("operand of unary op must parse as a binary op, got: {:#?}", unary_op.operand),
    };

    assert_eq!(Operator::Index, operand_bin_op.op);
}
