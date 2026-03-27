use crate::ast::Annotation;
use crate::ast::ElseBranch;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::MatchPattern;
use crate::ast::Stmt;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Separator;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MatchBlock<B, A = Span>
where
    A: Annotation
{
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub of_span: Span,
    
    pub cond_expr: Expr<A>,

    pub branches: Vec<MatchBlockBranch<B, A>>,
    pub else_branch: Option<ElseBranch<B>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub end_span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

pub type MatchExpr<A = Span> = MatchBlock<Expr<A>, A>;
pub type MatchStmt<A = Span> = MatchBlock<Stmt<A>, A>;

impl MatchExpr {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut group = Self::parse_block_group(tokens)?;
        
        let mut branches = Vec::new();
        let else_branch = Self::parse_branches(&mut group.tokens, &mut branches)?;

        group.finish_block(branches, else_branch)
    }
    
    fn parse_branches(
        tokens: &mut TokenStream,
        branches: &mut Vec<MatchBlockBranch<Expr>>
    ) -> ParseResult<Option<ElseBranch<Expr>>> {
        let expect_else = if tokens.look_ahead().match_one(Keyword::Else).is_some() { 
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
        let mut group = Self::parse_block_group(tokens)?;

        let mut branches = Vec::new();
        let expect_else = if tokens.look_ahead().match_one(Keyword::Else).is_some() {
            true
        } else {
            loop {
                let pattern = MatchPattern::parse(&mut group.tokens)?;
                let binding = Ident::try_parse(&mut group.tokens)?;
                group.tokens.match_one(Separator::Colon)?;

                match Stmt::parse(&mut group.tokens) {
                    Ok(branch_stmt) => {
                        branches.push(MatchBlockBranch::new(pattern, binding, branch_stmt))
                    },

                    // if the first branch is only valid as an expression, convert it into a match-expr
                    // branch and parse the rest of the branches as expressions too
                    Err(TracedError { err: ParseError::IsExpr(illegal), .. }) => {
                        let illegal_branch = MatchBlockBranch::new(pattern, binding, *illegal.0);
                        return Err(Self::fail_as_expr_with_branch(group, illegal_branch, branches));
                    }

                    Err(other) => {
                        return Err(other);
                    }
                };

                match Self::match_end_of_branches(&mut group.tokens) {
                    MatchBranchNextItem::Branch => continue,
                    MatchBranchNextItem::ElseBranch => break true,
                    MatchBranchNextItem::End => break false,
                }
            }
        };

        let else_branch = if expect_else {
            let else_kw = group.tokens.match_one(Keyword::Else)?;

            match Stmt::parse(&mut group.tokens) {
                Err(TracedError { err: ParseError::IsExpr(illegal), .. }) => {
                    let illegal_else = ElseBranch::new(else_kw.into_span(), *illegal.0);
                    group.tokens.match_one_maybe(Separator::Semicolon);
                    return Err(Self::fail_as_expr_with_else(group, illegal_else, branches));
                }
                Err(err) => {
                    return Err(err);
                }
                Ok(else_stmt) => {
                    group.tokens.match_one_maybe(Separator::Semicolon);
                    Some(ElseBranch::new(else_kw.into_span(), else_stmt))
                },
            }
        } else {
            None
        };
        
        group.finish_block(branches, else_branch)
    }

    fn branches_to_exprs(
        illegal: &Expr,
        stmt_branches: Vec<MatchBlockBranch<Stmt>>,
    ) -> ParseResult<Vec<MatchBlockBranch<Expr>>> {
        let mut expr_branches = Vec::new();

        for branch in stmt_branches {
            match branch.item.to_expr() {
                Some(branch_expr) => {
                    expr_branches.push(MatchBlockBranch::new(
                        branch.pattern,
                        branch.binding,
                        branch_expr
                    ));
                },

                // if any previous branch was not a valid expression, this match block
                // cannot be a valid expression at all
                None => {
                    return Err(ParseError::illegal_expr(illegal.clone()).into());
                }
            }
        }

        Ok(expr_branches)
    }

    // parsing failed because an item expected to be a statement was an expression. attempt to
    // reinterpret all previously parsed statement branches as expressions, and if they are
    // valid, return an IsExpr indicating the caller may attempt to continue with this expression.
    fn fail_as_expr_with_branch(
        mut group: MatchBlockGroup,
        illegal_branch: MatchBlockBranch<Expr>,
        branches: Vec<MatchBlockBranch<Stmt>>,
    ) -> TracedError<ParseError> {
        let mut expr_branches = match Self::branches_to_exprs(&illegal_branch.item, branches) {
            Err(err) => return err,
            Ok(branches) => branches,
        };

        expr_branches.push(illegal_branch);

        let else_branch = match Self::match_end_of_branches(&mut group.tokens) {
            MatchBranchNextItem::Branch => {
                match MatchExpr::parse_branches(&mut group.tokens, &mut expr_branches) {
                    Err(err) => return err,
                    Ok(expr) => expr,
                }
            }
            MatchBranchNextItem::ElseBranch => {
                match MatchExpr::parse_else_item(&mut group.tokens) {
                    Ok(else_branch) => Some(else_branch),
                    Err(err) => return err,
                }
            }
            MatchBranchNextItem::End => None,
        };

        let match_expr = match group.finish_block(expr_branches, else_branch) {
            Err(err) => return err,
            Ok(expr) => expr,
        };

        ParseError::is_expr(Expr::from(match_expr)).into()
    }

    fn fail_as_expr_with_else(
        group: MatchBlockGroup,
        illegal_else: ElseBranch<Expr>,
        branches: Vec<MatchBlockBranch<Stmt>>,
    ) -> TracedError<ParseError> {
        let expr_branches = match Self::branches_to_exprs(&illegal_else.item, branches) {
            Err(err) => return err,
            Ok(branches) => branches,
        };

        let match_expr = match group.finish_block(expr_branches, Some(illegal_else)) {
            Err(err) => return err,
            Ok(expr) => expr,
        };

        ParseError::is_expr(Expr::from(match_expr)).into()
    }
}

impl<B> MatchBlock<B> {
    fn parse_block_group(tokens: &mut TokenStream) -> ParseResult<MatchBlockGroup> {
        let block_group = DelimitedGroup::parse(tokens, DelimiterPair::MatchEnd)?;

        let kw_span = block_group.open.clone();
        let end_span = block_group.close.clone();

        let mut inner_tokens = block_group.into_inner_tokens();

        let cond_expr = Expr::parse(&mut inner_tokens)?;
        let of_span = inner_tokens.match_one(Keyword::Of)?.into_span();
        
        Ok(MatchBlockGroup {
            cond_expr,
            tokens: inner_tokens,
            span: kw_span.to(&end_span),
            kw_span,
            of_span,
            end_span,
        })
    }
    
    pub(crate) fn match_end_of_branches(tokens: &mut TokenStream) -> MatchBranchNextItem {
        match tokens.look_ahead().match_one(Keyword::Else | Separator::Semicolon) {
            // semicolon separator
            Some(tt) if tt.is_separator(Separator::Semicolon) => {
                tokens.advance(1);

                match tokens.look_ahead().next() {
                    Some(tt) if tt.is_keyword(Keyword::Else) => {
                        MatchBranchNextItem::ElseBranch
                    },
                    
                    None => MatchBranchNextItem::End,
                    
                    _ =>  MatchBranchNextItem::Branch,
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

impl<B: Parse> MatchBlock<B> {
    pub(crate) fn parse_else_item(tokens: &mut TokenStream) -> ParseResult<ElseBranch<B>> {
        let branch = ElseBranch::parse(tokens)?;
        tokens.match_one_maybe(Separator::Semicolon);
        
        Ok(branch)
    }
}

pub(crate) enum MatchBranchNextItem {
    Branch,
    ElseBranch,
    End,
}

impl<B, A> fmt::Display for MatchBlock<B, A>
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
            writeln!(f, "\telse {};", else_branch.item)?;
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
                binding: branch.binding.clone(),
                span: branch.span.clone(),
            })
        }

        let else_branch = match self.else_branch.as_ref() {
            Some(branch) => {
                Some(ElseBranch::new(branch.else_kw_span.clone(), branch.item.to_expr()?))
            },
            None => None,
        };

        Some(MatchExpr {
            kw_span: self.kw_span.clone(),
            of_span: self.of_span.clone(),
            cond_expr: self.cond_expr.clone(),
            else_branch,
            branches,
            annotation: self.annotation.clone(),
            end_span: self.end_span.clone(),
        })
    }
}

impl<B, A> Spanned for MatchBlock<B, A>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

struct MatchBlockGroup {
    kw_span: Span,
    of_span: Span,
    tokens: TokenStream,
    span: Span,
    cond_expr: Expr,
    end_span: Span,
}

impl MatchBlockGroup {
    pub fn finish_block<B>(self,
        branches: Vec<MatchBlockBranch<B>>,
        else_branch: Option<ElseBranch<B>>
    ) -> ParseResult<MatchBlock<B>> {
        self.tokens.finish()?;
        
        Ok(MatchBlock {
            kw_span: self.kw_span,
            end_span: self.end_span,
            of_span: self.of_span,
            cond_expr: self.cond_expr,
            annotation: self.span,
            branches,
            else_branch,
        })
    } 
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MatchBlockBranch<B, A = Span>
where
    A: Annotation
{
    pub pattern: MatchPattern<A>,
    pub binding: Option<Ident>,

    pub item: B,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<B> MatchBlockBranch<B>
where
    B: Spanned
{
    pub fn new(pattern: MatchPattern, binding: Option<Ident>, item: B) -> Self {
        let span = pattern.span().to(&item);
        
        MatchBlockBranch {
            span,
            item,
            binding,
            pattern,
        }
    }
}

impl<B> Parse for MatchBlockBranch<B>
where
    B: Parse + Spanned
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let pattern = MatchPattern::parse(tokens)?;
        let binding = Ident::try_parse(tokens)?;
        
        tokens.match_one(Separator::Colon)?;

        let item = B::parse(tokens)?;

        Ok(MatchBlockBranch {
            span: pattern.span().to(item.span()),
            binding,
            pattern,
            item,
        })
    }
}

impl<B, A> Spanned for MatchBlockBranch<B, A>
where
    A: Annotation
{
    fn span(&self) -> &Span {
        &self.span
    }
}
