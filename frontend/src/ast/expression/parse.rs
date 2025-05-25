mod operator;

use crate::ast::expression::parse::operator::resolve_ops_by_precedence;
use crate::ast::expression::parse::operator::OperatorPart;
use crate::ast::expression::parse::operator::SymbolOperator;
use crate::ast::match_block::MatchExpr;
use crate::ast::operators::*;
use crate::ast::type_name::TypeName;
use crate::ast::AnonymousFunctionDef;
use crate::ast::ArgList;
use crate::ast::Block;
use crate::ast::CaseExpr;
use crate::ast::CollectionCtor;
use crate::ast::Exit;
use crate::ast::Expr;
use crate::ast::IfCond;
use crate::ast::Literal;
use crate::ast::ObjectCtor;
use crate::ast::ObjectCtorArgs;
use crate::ast::ObjectCtorMember;
use crate::ast::Raise;
use crate::ast::TypeArgList;
use crate::ast::TypeList;
use crate::parse::*;
use crate::token_tree::*;
use crate::Keyword;
use terapascal_common::span::*;
use terapascal_common::TracedError;
use std::fmt;

fn parse_identifier(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    // the context of an identifier expr should be the first part of the
    // identifier (if it has multiple parts)
    match tokens.match_one(Matcher::AnyIdent)? {
        TokenTree::Ident(ident) => {
            let span = ident.span.clone();
            Ok(Expr::Ident(ident, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_string(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    match tokens.match_one(Matcher::AnyLiteralString)? {
        TokenTree::String { value, span } => {
            let str_lit = Literal::String(value);
            Ok(Expr::Literal(str_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_integer(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    match tokens.match_one(Matcher::AnyLiteralInteger)? {
        TokenTree::IntNumber { value, span } => {
            let int_lit = Literal::Integer(value);
            Ok(Expr::Literal(int_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_real(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    match tokens.match_one(Matcher::AnyLiteralReal)? {
        TokenTree::RealNumber { value, span } => {
            let real_lit = Literal::Real(value);
            Ok(Expr::Literal(real_lit, span))
        },

        _ => unreachable!(),
    }
}

fn parse_literal_bool(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    let kw = tokens.match_one(Keyword::True.or(Keyword::False))?;
    let val = kw.is_keyword(Keyword::True);
    let expr = Expr::Literal(Literal::Boolean(val), kw.span().clone());
    Ok(expr)
}

fn parse_size_of(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    let kw = tokens.match_one(Keyword::SizeOf)?;

    let (close_bracket, mut ty_tokens) = match tokens.match_one(DelimiterPair::Bracket)? {
        TokenTree::Delimited(group) => (group.close.clone(), group.to_inner_tokens()),
        _ => unreachable!(),
    };

    let ty = TypeName::parse(&mut ty_tokens)?;
    ty_tokens.finish()?;

    let span = kw.span().to(close_bracket.span());

    Ok(Expr::Literal(Literal::SizeOf(Box::new(ty)), span))
}

fn parse_default(tokens: &mut TokenStream) -> ParseResult<Expr<Span>> {
    let kw = tokens.match_one(Keyword::Default)?;

    let (ty, span) = match tokens.match_one_maybe(DelimiterPair::Bracket) {
        Some(TokenTree::Delimited(group)) => {
            let span = kw.span().to(group.close.span());

            let mut ty_tokens = group.to_inner_tokens();
            let ty_name = TypeName::parse(&mut ty_tokens)?;
            ty_tokens.finish()?;

            (ty_name, span)
        },
        
        Some(..) => unreachable!(),

        None => {
            let no_type = TypeName::Unspecified(kw.span().clone());
            (no_type, kw.into_span())
        }
    };

    Ok(Expr::Literal(Literal::DefaultValue(Box::new(ty)), span))
}

fn parse_type_info_expr(tokens: &mut TokenStream) -> ParseResult<Expr> {
    let kw = tokens.match_one(Keyword::TypeInfo)?;
    let typename_group = tokens.match_one(DelimiterPair::Bracket)?
        .into_delimited_group()
        .unwrap();

    let span = kw.span().to(&typename_group.close);
    
    let mut typename_tokens = typename_group.to_inner_tokens();
    let typename = TypeName::parse(&mut typename_tokens)?;
    typename_tokens.finish()?;

    Ok(Expr::Literal(Literal::TypeInfo(Box::new(typename)), span))
}

pub fn parse_case_expr(tokens: &mut TokenStream) -> ParseResult<Expr> {
    let case = CaseExpr::parse_expr(tokens)?;
    let expr = Expr::Case(Box::new(case));

    Ok(expr)
}

#[derive(Debug, Clone)]
enum CompoundExpressionPart {
    Operand(Expr<Span>),
    Operator(OperatorPart),
}

pub struct CompoundExpressionParser<'tokens> {
    tokens: &'tokens mut TokenStream,
    last_was_operand: bool,
    parts: Vec<CompoundExpressionPart>,
}

impl fmt::Display for CompoundExpressionPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompoundExpressionPart::Operand(expr) => {
                write!(f, "expr `{}`", expr)
            },
            CompoundExpressionPart::Operator(OperatorPart::OperatorSymbol(os)) => {
                write!(f, "operator symbol `{}` ({})", os.op, os.pos)
            },
            CompoundExpressionPart::Operator(OperatorPart::Call { args, type_args }) => {
                write!(f, "call: ")?;
                if let Some(type_args) = type_args {
                    write!(f, "{}", type_args)?;
                }

                write!(f, "], arg list (")?;
                for (i, arg) in args.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")?;
                Ok(())
            },
            
            CompoundExpressionPart::Operator(OperatorPart::ObjectCtor { args, type_args }) => {
                write!(f, "ctor")?;
                if let Some(type_args) = type_args {
                    write!(f, " {}", type_args)?;
                }
                
                write!(f, ": {args}")
            },

            CompoundExpressionPart::Operator(OperatorPart::AsCast { ty, .. }) => {
                write!(f, "cast to {}", ty)
            },

            CompoundExpressionPart::Operator(OperatorPart::WithTypeArgs { arg_types, .. }) => {
                write!(f, "specialization with type args {}", arg_types)
            },
        }
    }
}

impl<'tokens> CompoundExpressionParser<'tokens> {
    pub fn new(tokens: &'tokens mut TokenStream) -> Self {
        CompoundExpressionParser {
            tokens,
            last_was_operand: false,
            parts: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult<Expr<Span>> {
        loop {
            // an operand can't be followed by another operand, so each time we finish
            // an operand, the next thing must be a member access, a list of arguments to
            // the function the operand referred to, an array element access, or a binary
            // operator connecting this operand to the next one
            let more = if !self.last_was_operand {
                self.parse_operand()?
            } else {
                self.parse_operator()?
            };

            if !more {
                break {
                    if self.parts.is_empty() {
                        let expected = Matcher::ExprOperandStart;
                        return Err(TracedError::trace(match self.tokens.look_ahead().next() {
                            Some(unexpected) => {
                                ParseError::UnexpectedToken(Box::new(unexpected.clone()), Some(expected))
                            },
                            None => {
                                let after = self.tokens.context().clone();
                                ParseError::UnexpectedEOF(expected, after)
                            },
                        }));
                    }

                    resolve_ops_by_precedence(self.parts)
                };
            }
        }
    }

    fn parse_operand(&mut self) -> ParseResult<bool> {
        self.last_was_operand = true;

        let mut look_ahead = self.tokens.look_ahead();
        match look_ahead.match_one(Matcher::ExprOperandStart) {
            Some(TokenTree::Delimited(group)) => {
                match group.delim {
                    // operand is a () group: must be a sub-expr or unnamed ctor
                    DelimiterPair::Bracket => {
                        let group = group.clone();
                        self.parse_bracket_group(group)?;
                    },

                    // operand is a [] group: must be a collection ctor
                    DelimiterPair::SquareBracket => {
                        let ctor = CollectionCtor::parse(self.tokens)?;
                        let expr = Expr::from(ctor);
                        self.push_operand(expr);
                    },

                    // operand is a begin/end group: must be a block
                    DelimiterPair::BeginEnd => {
                        let block = Block::parse(self.tokens)?;
                        let expr = Expr::from(block);
                        self.push_operand(expr);
                    },

                    DelimiterPair::CaseEnd => {
                        let case = CaseExpr::parse_expr(&mut self.tokens)?;
                        self.push_operand(Expr::from(case))
                    },

                    DelimiterPair::MatchEnd => {
                        let match_expr = MatchExpr::parse_expr(&mut self.tokens)?;
                        self.push_operand(Expr::from(match_expr))
                    },
                }
            },

            // operand is an unsafe block
            Some(tt) if tt.is_keyword(Keyword::Unsafe) => {
                let block = Block::parse(self.tokens)?;
                let expr = Expr::from(block);
                self.push_operand(expr);
            },

            // it's an operator, but thanks to the match we know this operator
            // is valid in prefix position, so it's part of this expr
            Some(TokenTree::Operator { .. }) => {
                // we need to parse another operand after this!
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.push_operator_token(op, Position::Prefix);
            },

            // the simple values
            Some(TokenTree::Ident(_)) => {
                let expr = parse_identifier(self.tokens)?;
                self.push_operand(expr);
            },

            Some(TokenTree::String { .. }) => {
                let expr = parse_literal_string(self.tokens)?;
                self.push_operand(expr);
            },

            Some(TokenTree::IntNumber { .. }) => {
                let expr = parse_literal_integer(self.tokens)?;
                self.push_operand(expr);
            },

            Some(tt) if tt.is_keyword(Keyword::Nil) => {
                let nil_token = self.tokens.next().unwrap();
                self.push_operand(Expr::Literal(Literal::Nil, nil_token.span().clone()));
            },

            Some(tt) if tt.is_keyword(Keyword::SizeOf) => {
                let size_of_expr = parse_size_of(self.tokens)?;
                self.push_operand(size_of_expr);
            },

            Some(tt) if tt.is_keyword(Keyword::Default) => {
                let default_expr = parse_default(self.tokens)?;
                self.push_operand(default_expr);
            },

            Some(tt) if tt.is_keyword(Keyword::If) => {
                let cond = IfCond::parse_expr(self.tokens)?;
                let expr = Expr::from(cond);
                self.push_operand(expr);
            },

            Some(tt) if tt.is_keyword(Keyword::True) || tt.is_keyword(Keyword::False) => {
                let expr = parse_literal_bool(self.tokens)?;
                self.push_operand(expr);
            },

            Some(tt) if tt.is_keyword(Keyword::Raise) => {
                let raise = Raise::parse(self.tokens)?;
                let expr = Expr::from(raise);
                self.push_operand(expr);
            },

            Some(tt) if tt.is_keyword(Keyword::Exit) => {
                let raise = Exit::parse(self.tokens)?;
                let expr = Expr::from(raise);
                self.push_operand(expr);
            },
            
            Some(tt) if tt.is_keyword(Keyword::TypeInfo) => {
                let expr = parse_type_info_expr(self.tokens)?;
                self.push_operand(expr);
            }

            Some(tt) if tt.is_delimited(DelimiterPair::CaseEnd) => {
                let expr = parse_case_expr(self.tokens)?;
                self.push_operand(expr);
            },

            Some(TokenTree::RealNumber { .. }) => {
                let expr = parse_literal_real(self.tokens)?;
                self.push_operand(expr);
            },

            Some(tt) if tt.is_keyword(Keyword::Function) || tt.is_keyword(Keyword::Procedure) || tt.is_keyword(Keyword::Lambda) => {
                let func_def = AnonymousFunctionDef::parse(self.tokens)?;
                self.push_operand(Expr::from(func_def))
            },

            Some(x) => unreachable!("got {:?} which is excluded by pattern", x),

            // next token is not valid as part of an operand, so this expr
            // must end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn parse_bracket_group(&mut self, group: DelimitedGroup) -> ParseResult<()> {
        let open_bracket = group.open.clone();
        let close_bracket = group.close.clone();

        let mut tokens = group.to_inner_tokens();

        let mut sub_expr;

        // if the inner group is completely empty, the only valid way to parse it is as an empty object ctor
        if tokens.look_ahead().next().is_none() {
            sub_expr = Expr::from(ObjectCtor {
                type_expr: None,
                annotation: open_bracket.to(&close_bracket),
                args: ObjectCtorArgs {
                    span: open_bracket.to(&close_bracket),
                    members: Vec::new(),
                },
                type_args: None,
            });
        } else {
            sub_expr = Expr::parse(&mut tokens)?;

            // if the group is in the format `( some_ident: some_other_expr )` then it's not just a sub-expression,
            // it's an object constructor group without a preceding object name
            if let Some(item_ident) = sub_expr.as_ident() {
                if tokens.match_one_maybe(Separator::Colon).is_some() {
                    let first_item_val = Expr::parse(&mut tokens)?;
                    let mut items = vec![ObjectCtorMember {
                        ident: item_ident.clone(),
                        span: item_ident.span().to(first_item_val.span()),
                        value: first_item_val,
                    }];

                    // parse any items following a subsequent semicolon using the normal ctor parser
                    if tokens.match_one_maybe(Separator::Semicolon).is_some() {
                        let rest_items = ObjectCtorMember::parse_seq(&mut tokens)?;
                        items.extend(rest_items);
                    }

                    sub_expr = Expr::from(ObjectCtor {
                        type_expr: None,
                        annotation: open_bracket.to(&close_bracket),
                        args: ObjectCtorArgs {
                            span: open_bracket.to(&close_bracket),
                            members: items,
                        },
                        type_args: None,
                    });
                }
            }
        }

        tokens.finish()?;

        self.tokens.advance(1);
        self.push_operand(sub_expr);

        Ok(())
    }

    fn push_operand(&mut self, expr: Expr<Span>) {
        let part = CompoundExpressionPart::Operand(expr);
        self.parts.push(part);
    }

    fn parse_invocation(&mut self, type_args: Option<TypeArgList>) -> ParseResult<()> {
        let match_arg_list = Matcher::from(DelimiterPair::Bracket);
        
        let mut inner_tokens = match self.tokens.look_ahead().next().cloned() {
            Some(TokenTree::Delimited(group)) => {
                group.to_inner_tokens()
            },

            Some(bad) => {
                return Err(TracedError::trace(ParseError::UnexpectedToken(
                    Box::new(bad),
                    Some(match_arg_list),
                )))
            },
            None => {
                return Err(TracedError::trace(ParseError::UnexpectedEOF(
                    match_arg_list,
                    self.tokens.context().clone(),
                )))
            },
        };

        let ctor_matcher = Matcher::AnyIdent + Separator::Colon;

        // if the next two tokens are in the form `a:` AND the last operand is
        // an ident then it has to be an object constructor list instead of a call
        let is_ctor_ahead = inner_tokens
            .look_ahead()
            .match_sequence(ctor_matcher)
            .is_some();

        if is_ctor_ahead {
            let args = ObjectCtorArgs::parse(&mut self.tokens)?;
            
            self.parts.push(CompoundExpressionPart::Operator(OperatorPart::ObjectCtor {
                args,
                type_args
            }));
        } else {
            let args = ArgList::parse(&mut self.tokens)?;

            self.parts.push(CompoundExpressionPart::Operator(OperatorPart::Call { 
                args, 
                type_args
            }));
        }

        self.last_was_operand = true;

        Ok(())
    }

    fn parse_member_access(&mut self) -> ParseResult<()> {
        let member_op_tt = self.tokens.match_one(Operator::Period)?;

        self.push_operator_token(member_op_tt, Position::Binary);

        let member_ident = self
            .tokens
            .match_one(Matcher::AnyIdent)?
            .into_ident()
            .unwrap();

        self.push_operand(Expr::from(member_ident));

        self.last_was_operand = true;

        Ok(())
    }

    fn parse_operator(&mut self) -> ParseResult<bool> {
        let operator_matcher = Matcher::AnyOperatorInPosition(Position::Binary)
            | Matcher::AnyOperatorInPosition(Position::Postfix);

        let match_after_operand = operator_matcher
            | DelimiterPair::SquareBracket // array element access or explicit generic args
            | DelimiterPair::Bracket // function call argument list
            | Operator::Period; // member access

        let mut look_ahead = self.tokens.look_ahead();

        match look_ahead.match_one(match_after_operand) {
            // a bracket group starting in operator position is always a call or a ctor call
            // e.g. x.y.z(a, b, c) or A(b: 2)
            Some(tt) if tt.is_delimited(DelimiterPair::Bracket) => {
                let ty_args = None;
                self.parse_invocation(ty_args)?;
            },

            // a square bracket group in operator position may be an indexer:
            // * `x := a[1];`
            // * `x := b.Y()[4123];`
            //
            // or a type param list:
            // * `x := A[Integer]();`
            // * `DoSomething[Byte](255);`
            Some(tt) if tt.is_delimited(DelimiterPair::SquareBracket) => {
                // if the square bracket group is followed by a bracket group, it must be a generic
                // function invocation or ctor call
                if look_ahead.match_one(DelimiterPair::Bracket).is_some() {
                    self.parse_invocation_with_type_args()?;
                } else {
                    self.parse_indexer()?;
                }
            },

            Some(TokenTree::Operator {
                op: Operator::Period,
                ..
            }) => {
                self.parse_member_access()?;
            },

            Some(TokenTree::Operator { .. }) => {
                let op_tt = self.tokens.match_one(Matcher::AnyOperator)?;
                let op = op_tt.as_operator().unwrap();

                // special behaviour for "as" operator because it needs to parse a typename too
                if op == Operator::As {
                    let as_typename = TypeName::parse(self.tokens)?;

                    self.last_was_operand = true;
                    self.push_operator_cast(op_tt, as_typename);
                } else if op == Operator::With {
                    let with_group = DelimitedGroup::parse(
                        self.tokens, 
                        DelimiterPair::SquareBracket
                    )?;
                    
                    let with_span = with_group.span.clone();
                    
                    let mut with_tokens = with_group.to_inner_tokens();
                    let mut arg_types = Vec::new();
                    loop {
                        if !arg_types.is_empty() {
                            if with_tokens.match_one_maybe(Separator::Comma).is_none() {
                                break;
                            }
                        }
                        
                        let arg_type = TypeName::parse(&mut with_tokens)?;
                        arg_types.push(arg_type);
                    }

                    with_tokens.finish()?;
                    
                    let arg_types = TypeList::new(arg_types, with_span);
                    
                    self.last_was_operand = true;
                    self.push_operator_with(op_tt, arg_types);
                } else if op.is_valid_in_pos(Position::Postfix) {
                    // assumes there are no postfix operators that are also valid in infix position
                    // expect another operator
                    self.last_was_operand = true;
                    self.push_operator_token(op_tt, Position::Postfix);
                } else {
                    // expect another operand afterwards
                    self.last_was_operand = false;
                    self.push_operator_token(op_tt, Position::Binary);
                }
            },

            Some(illegal) => panic!("pattern excludes anything else: got {}", illegal),

            // nothing following the last operand, which is fine, just end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn parse_invocation_with_type_args(&mut self) -> ParseResult<()> {
        let type_args = TypeList::parse_type_args(self.tokens)?;

        self.parse_invocation(Some(type_args))?;

        Ok(())
    }

    fn parse_indexer(&mut self) -> ParseResult<()> {
        let index_group = match self.tokens.match_one(DelimiterPair::SquareBracket)? {
            TokenTree::Delimited(group) => group,
            _ => unreachable!(),
        };

        let index_span = index_group.span.clone();

        let index = {
            let mut index_tokens = index_group.to_inner_tokens();
            let index_expr = Expr::parse(&mut index_tokens)?;
            index_tokens.finish()?;

            index_expr
        };

        let indexer_part = OperatorPart::OperatorSymbol(SymbolOperator {
            op: Operator::Index,
            pos: Position::Binary,
            span: index_span,
        });
        self.parts
            .push(CompoundExpressionPart::Operator(indexer_part));

        self.push_operand(index);

        self.last_was_operand = true;

        Ok(())
    }

    fn push_operator_cast(&mut self, op_tt: TokenTree, ty: TypeName) {
        let op_as = OperatorPart::AsCast {
            kw_span: op_tt.into_span(),
            ty,
        };

        let part = CompoundExpressionPart::Operator(op_as);
        self.parts.push(part);
    }

    fn push_operator_with(&mut self, op_tt: TokenTree, arg_types: TypeList<TypeName>) {
        let op_with = OperatorPart::WithTypeArgs {
            kw_span: op_tt.into_span(),
            arg_types,
        };

        let part = CompoundExpressionPart::Operator(op_with);
        self.parts.push(part);
    }

    fn push_operator_token(&mut self, op_token: TokenTree, pos: Position) {
        let op_token = OperatorPart::OperatorSymbol(SymbolOperator {
            op: op_token.as_operator().unwrap(),
            pos,
            span: op_token.span().clone(),
        });

        let part = CompoundExpressionPart::Operator(op_token);
        self.parts.push(part);
    }
}
