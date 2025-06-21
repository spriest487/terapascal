use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::parse::*;
use crate::token_tree::*;
use crate::Keyword;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::*;
use terapascal_common::TracedError;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Block<A: Annotation = Span> {
    pub stmts: Vec<Stmt<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,

    // the final expr of the block which determines its value.
    // we can identify this during parsing if the last "stmt" in the block is an
    // expr which can't be parsed as a valid standalone stmt. otherwise, this gets
    // populated during typechecking.
    // e.g. a function call at the end a block may be the the block output depending on the return
    // type of the function, but we don't know that until typechecking. a value on its own, however,
    // would HAVE to be the block output to be valid!
    pub output: Option<Expr<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub unsafe_kw: Option<Span>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub begin: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub end: Span,
}

impl<A: Annotation> Block<A> {
    pub fn single_stmt(stmt: Stmt<A>) -> Self {
        Self {
            annotation: stmt.annotation().clone(),
            begin: stmt.annotation().span().clone(),
            end: stmt.annotation().span().clone(),
            stmts: vec![stmt],
            unsafe_kw: None,
            output: None,
        }
    }
}

impl Block<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let unsafe_kw = tokens
            .match_one_maybe(Keyword::Unsafe)
            .map(|unsafe_tt| unsafe_tt.into_span());

        let body_group = DelimitedGroup::parse(tokens, DelimiterPair::BeginEnd)?;

        let begin = body_group.open.clone();
        let end = body_group.close.clone();
        
        let span = match &unsafe_kw {
            Some(span) => span.to(&body_group.close),
            None => body_group.span.clone(),
        };

        let stmt_tokens = body_group.into_inner_tokens();
        
        let block = Self {
            stmts: Vec::new(),
            annotation: span,

            output: None,

            begin,
            end,

            unsafe_kw,
        };

        let block = parse_block_stmts(stmt_tokens, block)?;

        Ok(block)
    }

    // convert block-as-stmt into an block-as-expr
    // todo: should these be two different types?
    pub fn to_expr(&self) -> Option<Self> {
        if self.output.is_some() {
            // block that already had an output expr
            return Some(self.clone());
        }

        // block where we can reinterpret the final stmt as an output expr?
        let final_stmt_expr = self
            .stmts
            .last()
            .and_then(|final_stmt| final_stmt.clone().to_expr());

        if let Some(output_expr) = final_stmt_expr {
            let mut statements = self.stmts.clone();
            statements.pop();

            return Some(Block {
                stmts: statements,
                output: Some(output_expr),
                annotation: self.annotation.clone(),
                unsafe_kw: self.unsafe_kw.clone(),
                begin: self.begin.clone(),
                end: self.end.clone(),
            });
        }

        // block that doesn't work as an expr
        None
    }
}

fn parse_block_stmts(
    mut tokens: TokenStream,
    mut block: Block,
) -> ParseResult<Block> {
    let mut errors = Vec::new();

    loop {
        if tokens.look_ahead().next().is_none() {
            break;
        }
        
        match parse_block_stmt_agg(&mut tokens) {
            Ok(BlockStatementParsedItem::Stmt(stmt)) => {
                block.stmts.push(stmt);
            }
            
            Ok(BlockStatementParsedItem::OutputExpr(expr)) => {
                if let Some(old_output) = block.output.take() {
                    let illegal = IllegalStatement(Box::new(old_output));
                    
                    errors.push(TracedError::from(ParseError::IsExpr(illegal)));
                }
                
                block.output = Some(expr);
            }
            
            Err(err) => {
                errors.push(err);
            }
        }

        // continue parsing after the next semicolon, if there is one
        let mut unexpected_after_stmt = Vec::new();
       
        let more = loop {
            match tokens.look_ahead().next().cloned() {
                Some(tt) => {
                    tokens.advance(1);

                    if tt.is_separator(Separator::Semicolon) {
                        break true;
                    }
                    
                    unexpected_after_stmt.push(tt);
                }

                None => {
                    break false;
                },
            };
        };

        if let Some(unexpected_span) = Span::range(&unexpected_after_stmt) {
            errors.push(TracedError::from(ParseError::UnexpectedTokens(
                unexpected_span,
                Some(Matcher::Separator(Separator::Semicolon))
            )));
        }
        
        if !more {
            break;
        }
    }

    if !block.stmts.is_empty() || block.output.is_some() {
        tokens.match_one_maybe(Separator::Semicolon);
    }

    if let Err(err) = tokens.finish() {
        errors.push(err);
    }

    let result = AggregateParseError::result(block, errors)
        .map_err(AggregateParseError::into_err)?;
    Ok(result)
}

enum BlockStatementParsedItem {
    Stmt(Stmt),
    OutputExpr(Expr),
}

fn parse_block_stmt_agg(tokens: &mut TokenStream) -> ParseResult<BlockStatementParsedItem> {
    // we want to be able to asser than when we reinterpret an invalid statement as the output
    // expr, it's actually the expr starting from the same token as where we tried to parse the
    // statement. if it isn't, a statement nested within this has failed to properly handle
    // an InvalidStatement error itself!
    let stmt_start = tokens
        .look_ahead()
        .next()
        .map(|tt| tt.span().start);

    match Stmt::parse(tokens) {
        Ok(stmt) => {
            Ok(BlockStatementParsedItem::Stmt(stmt))
        },

        Err(traced_err) => match traced_err.err {
            // if the final stmt is invalid as a stmt but still a valid
            // expr, assume it's the block output. some expressions (eg calls) are
            // always valid as statements regardless of type, so in some cases the block
            // output can't be determined until typechecking
            err @ ParseError::IsExpr(..) => {
                let mut ahead = tokens.look_ahead();

                // if there's more statements after this, we can't use it as the output
                let stmt_after_tokens = ahead
                    .match_sequence(Separator::Semicolon + Matcher::AnyToken);

                if stmt_after_tokens.is_some() {
                    return Err(TracedError::trace(err));
                }

                let ParseError::IsExpr(IllegalStatement(bad_expr)) = err else {
                    unreachable!()
                };

                assert_eq!(
                    Some(bad_expr.span().start),
                    stmt_start,
                    "expression @ {} used as block output has the wrong position (child statement failed to handle invalid statement correctly)",
                    bad_expr.span()
                );

                Ok(BlockStatementParsedItem::OutputExpr(*bad_expr))
            }

            // failed for other reasons, this is an actual error
            _ => Err(traced_err),
        },
    }
}

impl<A: Annotation> fmt::Display for Block<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "begin")?;
        
        if !self.stmts.is_empty() || self.output.is_some() {
            writeln!(f)?;

            for (i, stmt) in self.stmts.iter().enumerate() {
                write!(f, "{}", stmt)?;

                // only add a ; on the last stmt if there's also an output expr
                if i < self.stmts.len() - 1 || self.output.is_some() {
                    write!(f, ";")?;
                }

                writeln!(f, "")?;
            }

            if let Some(output) = &self.output {
                write!(f, "{}", output)?;
            }
        } else {
            write!(f, " ")?;
        }

        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for Block<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
