use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::TypeNamePattern;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MatchBlock<A, B>
where
    A: Annotation
{
    pub cond_expr: Expr<A>,

    pub branches: Vec<MatchBlockBranch<A, B>>,
    pub else_branch: Option<B>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

pub type MatchExpr<A = Span> = MatchBlock<A, Expr<A>>;
pub type MatchStmt<A = Span> = MatchBlock<A, Stmt<A>>;

impl MatchExpr {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut block = Self::parse_block_group(tokens)?;
        
        let mut branches = Vec::new();
        let else_branch = Self::parse_branches(&mut block.tokens, &mut branches)?;

        block.tokens.finish()?;

        Ok(MatchBlock {
            cond_expr: block.cond_expr,
            branches,
            else_branch,
            annotation: block.span,
        })
    }
    
    fn parse_branches(
        tokens: &mut TokenStream,
        branches: &mut Vec<MatchBlockBranch<Span, Expr>>
    ) -> ParseResult<Option<Expr>> {
        let expect_else = if tokens.match_one_maybe(Keyword::Else).is_some() { 
            true 
        } else {
            loop {
                let branch = MatchBlockBranch::parse(tokens)?;
                branches.push(branch);

                match Self::match_end_of_branches(tokens) {
                    MatchBranchNextItem::Branch => continue,
                    MatchBranchNextItem::ElseBranch => break true,
                    MatchBranchNextItem::End => break false,
                }
            }
        };

        let else_branch = if expect_else {
            Some(Self::parse_else_item(tokens)?)
        } else {
            None
        };
        
        Ok(else_branch)
    }
}

impl MatchStmt {
    pub fn parse_stmt(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut block = Self::parse_block_group(tokens)?;

        let mut branches = Vec::new();
        let expect_else = if tokens.match_one_maybe(Keyword::Else).is_some() {
            true
        } else {
            loop {
                let pattern = TypeNamePattern::parse(&mut block.tokens)?;
                block.tokens.match_one(Separator::Colon)?;

                match Stmt::parse(&mut block.tokens) {
                    Ok(branch_stmt) => {
                        branches.push(MatchBlockBranch::new(pattern, branch_stmt))
                    },

                    // if the first branch is only valid as an expression, convert it into a match-expr
                    // branch and parse the rest of the branches as expressions too
                    Err(TracedError { err: ParseError::IsExpr(illegal), .. }) => {
                        let mut expr_branches = Vec::new();
                        for branch in branches {
                            match branch.item.to_expr() {
                                Some(branch_expr) => {
                                    expr_branches.push(MatchBlockBranch::new(branch.pattern, branch_expr));
                                },

                                // if any previous branch was not a valid expression, this match block
                                // cannot be a valid expression at all
                                None => {
                                    return Err(ParseError::illegal_expr(illegal.0).into());
                                }
                            }
                        }

                        expr_branches.push(MatchBlockBranch::new(pattern, *illegal.0));

                        let else_branch = match Self::match_end_of_branches(&mut block.tokens) {
                            MatchBranchNextItem::Branch => {
                                MatchExpr::parse_branches(&mut block.tokens, &mut expr_branches)?
                            }
                            MatchBranchNextItem::ElseBranch => {
                                Some(MatchExpr::parse_else_item(&mut block.tokens)?)
                            }
                            MatchBranchNextItem::End => None,
                        };

                        block.tokens.finish()?;

                        let match_expr = MatchExpr {
                            cond_expr: block.cond_expr,
                            branches: expr_branches,
                            else_branch,
                            annotation: block.span,
                        };

                        return Err(ParseError::is_expr(Expr::from(match_expr)).into());
                    }

                    Err(other) => {
                        return Err(other);
                    }
                };

                match Self::match_end_of_branches(&mut block.tokens) {
                    MatchBranchNextItem::Branch => continue,
                    MatchBranchNextItem::ElseBranch => break true,
                    MatchBranchNextItem::End => break false,
                }
            }
        };

        let else_branch = if expect_else {
            Some(Self::parse_else_item(&mut block.tokens)?)
        } else {
            None
        };

        block.tokens.finish()?;

        Ok(MatchBlock {
            cond_expr: block.cond_expr,
            branches,
            else_branch,
            annotation: block.span,
        })
    }
}

impl<B> MatchBlock<Span, B> {
    fn parse_block_group(tokens: &mut TokenStream) -> ParseResult<MatchBlockGroupItem> {
        let block_group = DelimitedGroup::parse(tokens, DelimiterPair::MatchEnd)?;

        let kw_span = block_group.open.clone();
        let end_span = block_group.close.clone();

        let mut inner_tokens = block_group.to_inner_tokens();

        let cond_expr = Expr::parse(&mut inner_tokens)?;
        inner_tokens.match_one(Keyword::Of)?;
        
        Ok(MatchBlockGroupItem {
            cond_expr,
            tokens: inner_tokens,
            span: kw_span.to(&end_span),
        })
    }
    
    pub(crate) fn match_end_of_branches(tokens: &mut TokenStream) -> MatchBranchNextItem {
        match tokens.match_one_maybe(Keyword::Else | Separator::Semicolon) {
            // semicolon separator
            Some(tt) if tt.is_separator(Separator::Semicolon) => {
                if tokens.match_one_maybe(Keyword::Else).is_some() {
                    MatchBranchNextItem::ElseBranch
                } else if tokens.look_ahead().next().is_none() {
                    MatchBranchNextItem::End
                } else {
                    MatchBranchNextItem::Branch
                }
            }

            // else without a separating semicolon
            Some(tt) if tt.is_keyword(Keyword::Else) => { 
                MatchBranchNextItem::ElseBranch
            }
            
            // neither (either next token is the end, or it's invalid)
            None => MatchBranchNextItem::End,

            // matcher excludes everything else
            Some(..) => unreachable!(),
        }
    }
}

impl<B: Parse> MatchBlock<Span, B> {
    pub(crate) fn parse_else_item(tokens: &mut TokenStream) -> ParseResult<B> {
        let item = B::parse(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);
        
        Ok(item)
    }
}

pub(crate) enum MatchBranchNextItem {
    Branch,
    ElseBranch,
    End,
}

impl<A, B> fmt::Display for MatchBlock<A, B>
where
    A: Annotation,
    B: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "match {} of", self.cond_expr)?;

        for branch in &self.branches {
            writeln!(f, "\t{}: {};", branch.pattern, branch.item)?;
        }

        if let Some(else_branch) = &self.else_branch {
            writeln!(f, "\telse {};", else_branch)?;
        }

        write!(f, "end")
    }
}

impl MatchStmt<Span> {
    pub fn to_expr(&self) -> Option<MatchExpr<Span>> {
        let mut branches = Vec::new();
        for branch in &self.branches {
            let item = branch.item.to_expr()?;
            branches.push(MatchBlockBranch {
                item,
                pattern: branch.pattern.clone(),
                span: branch.span.clone(),
            })
        }

        let else_branch = match self.else_branch.as_ref() {
            Some(else_stmt) => Some(else_stmt.to_expr()?),
            None => None,
        };

        Some(MatchExpr {
            cond_expr: self.cond_expr.clone(),
            else_branch,
            branches,
            annotation: self.annotation.clone(),
        })
    }
}

impl<A, B> Spanned for MatchBlock<A, B>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

struct MatchBlockGroupItem {
    tokens: TokenStream,
    span: Span,
    cond_expr: Expr,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MatchBlockBranch<A, B>
where
    A: Annotation
{
    pub pattern: A::Pattern,
    pub item: B,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<B> MatchBlockBranch<Span, B>
where
    B: Spanned
{
    pub fn new(pattern: TypeNamePattern, item: B) -> Self {
        let span = pattern.span().to(&item);
        
        MatchBlockBranch {
            span,
            item,
            pattern,
        }
    }
}

impl<B> Parse for MatchBlockBranch<Span, B>
where
    B: Parse + Spanned
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let pattern = TypeNamePattern::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;

        let item = B::parse(tokens)?;

        Ok(MatchBlockBranch {
            span: pattern.span().to(item.span()),
            pattern,
            item,
        })
    }
}

impl<A, B> Spanned for MatchBlockBranch<A, B>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        &self.span
    }
}
