#[cfg(test)]
mod test;

use crate::ast::Annotation;
use crate::ast::ElseBranch;
use crate::ast::Expr;
use crate::ast::MatchBranchNextItem;
use crate::ast::MatchExpr;
use crate::ast::MatchStmt;
use crate::ast::Stmt;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Separator;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

pub type CaseStmt<A = Span> = CaseBlock<Stmt<A>, A>;
pub type CaseExpr<A = Span> = CaseBlock<Expr<A>, A>;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct CaseBlock<B, A: Annotation = Span> {
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub of_span: Span,
    
    pub cond_expr: Box<Expr<A>>,
    pub branches: Vec<CaseBranch<B, A>>,
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

impl<B, A> Spanned for CaseBlock<B, A>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<B, A> fmt::Display for CaseBlock<B, A>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "case {} of", self.cond_expr)?;

        for branch in &self.branches {
            writeln!(f, "{};", branch)?;
        }

        if let Some(else_branch) = &self.else_branch {
            writeln!(f, "else {}", else_branch.item)?;
        }

        writeln!(f, "end")
    }
}

impl CaseExpr {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut group = CaseBlockGroup::parse(tokens)?;

        let mut branches = Vec::new();
        let else_branch = Self::parse_branches(&mut group.tokens, &mut branches)?;

        group.finish_block(branches, else_branch)
    }

    fn parse_branches(
        tokens: &mut TokenStream,
        branches: &mut Vec<CaseBranch<Expr>>
    ) -> ParseResult<Option<ElseBranch<Expr>>> {
        let expect_else = if tokens.look_ahead().match_one(Keyword::Else).is_some() {
            true
        } else {
            loop {
                let branch = CaseBranch::parse(tokens)?;
                branches.push(branch);

                match MatchExpr::match_end_of_branches(tokens) {
                    MatchBranchNextItem::Branch => continue,
                    MatchBranchNextItem::ElseBranch => break true,
                    MatchBranchNextItem::End => break false,
                }
            }
        };

        let else_branch = if expect_else {
            Some(MatchExpr::parse_else_item(tokens)?)
        } else {
            None
        };

        Ok(else_branch)
    }
}

impl CaseStmt {
    pub fn parse_stmt(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut group = CaseBlockGroup::parse(tokens)?;

        let mut branches = Vec::new();

        let expect_else = if group.tokens.look_ahead().match_one(Keyword::Else).is_some() { 
            true 
        } else {
            loop {
                let values = parse_case_values(&mut group.tokens)?;

                match Stmt::parse(&mut group.tokens) {
                    Ok(branch_stmt) => {
                        branches.push(CaseBranch::new(values, branch_stmt))
                    },

                    // if the first branch is only valid as an expression, convert it into a case-expr
                    // branch and parse the rest of the branches as expressions too
                    Err(TracedError { err: ParseError::IsExpr(illegal), .. }) => {
                        let mut expr_branches = Vec::new();
                        for branch in branches {
                            match branch.item.to_expr() {
                                Some(branch_expr) => {
                                    expr_branches.push(CaseBranch::new(branch.case_values, branch_expr))
                                },

                                // if any previous branch was not a valid expression, this case block
                                // cannot be a valid expression at all
                                None => {
                                    return Err(ParseError::illegal_expr(illegal.0).into());
                                }
                            }
                        }

                        expr_branches.push(CaseBranch::new(values, *illegal.0));

                        let else_branch = match MatchExpr::match_end_of_branches(&mut group.tokens) {
                            MatchBranchNextItem::Branch => {
                                CaseExpr::parse_branches(&mut group.tokens, &mut expr_branches)?
                            }
                            MatchBranchNextItem::ElseBranch => {
                                Some(MatchExpr::parse_else_item(&mut group.tokens)?)
                            }
                            MatchBranchNextItem::End => None,
                        };

                        let match_expr = group.finish_block(expr_branches, else_branch)?;

                        return Err(ParseError::is_expr(Expr::from(match_expr)).into());
                    }

                    Err(other) => {
                        return Err(other);
                    }
                };

                match MatchStmt::match_end_of_branches(&mut group.tokens) {
                    MatchBranchNextItem::Branch => continue,
                    MatchBranchNextItem::ElseBranch => break true,
                    MatchBranchNextItem::End => break false,
                }
            }
        };

        let else_branch = if expect_else {
            let else_branch = ElseBranch::parse(&mut group.tokens)?;
            group.tokens.match_one_maybe(Separator::Semicolon);

            Some(else_branch)
        } else {
            None
        };

        group.finish_block(branches, else_branch)
    }
}


#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct CaseBranch<Item, A: Annotation = Span> {
    pub case_values: Vec<Expr<A>>,
    pub item: Box<Item>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<Item, A> Spanned for CaseBranch<Item, A>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Item, A> fmt::Display for CaseBranch<Item, A>
where
    A: Annotation,
    Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in 0..self.case_values.len() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.case_values[i])?;
        }

        write!(f, ": {}", self.item)
    }
}

impl<Item> CaseBranch<Item>
where
    Item: Spanned,
{
    pub fn new(values: Vec<Expr>, item: impl Into<Box<Item>>) -> Self {
        let item = item.into();
        
        assert!(!values.is_empty());
        let span = Span::range(&values).unwrap().to(item.span());

        CaseBranch {
            case_values: values,
            item: item.into(),
            span,
        }
    }
}

fn parse_case_values(tokens: &mut TokenStream) -> ParseResult<Vec<Expr>> {
    let mut values = Vec::new();
    loop {
        let value = Expr::parse(tokens)?;
        values.push(value);

        if tokens.match_one_maybe(Separator::Comma).is_none() {
            break;
        }
    }

    tokens.match_one(Separator::Colon)?;

    Ok(values)
}

impl<Item> CaseBranch<Item>
where
    Item: Parse + Spanned,
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let values = parse_case_values(tokens)?;
        let item = Item::parse(tokens)?;

        Ok(CaseBranch::new(values, item))
    }
}

impl CaseStmt<Span> {
    pub fn to_expr(&self) -> Option<CaseExpr<Span>> {
        // must have an else branch that is a valid expr
        let else_branch = self.else_branch.as_ref()?;
        let else_expr = else_branch.item.to_expr()?;

        let mut branches = Vec::with_capacity(self.branches.len());
        for branch in &self.branches {
            let item = (*branch.item).clone().to_expr()?;
            branches.push(CaseBranch {
                case_values: branch.case_values.clone(),
                span: branch.span.clone(),
                item: Box::new(item),
            })
        }

        Some(CaseExpr {
            kw_span: self.kw_span.clone(),
            of_span: self.of_span.clone(),
            cond_expr: self.cond_expr.clone(),
            branches,
            else_branch: Some(ElseBranch::new(else_branch.else_kw_span.clone(), else_expr)),
            annotation: self.annotation.clone(),
            end_span: self.end_span.clone(),
        })
    }
}

struct CaseBlockGroup {
    kw_span: Span,
    of_span: Span,
    cond_expr: Expr,
    tokens: TokenStream,
    end_span: Span,
    span: Span,
}

impl CaseBlockGroup {
    pub fn finish_block<B>(self,
        branches: Vec<CaseBranch<B>>,
        else_branch: Option<ElseBranch<B>>
    ) -> ParseResult<CaseBlock<B>> {
        self.tokens.finish()?;

        Ok(CaseBlock {
            end_span: self.end_span,
            kw_span: self.kw_span,
            of_span: self.of_span,
            cond_expr: Box::new(self.cond_expr),
            annotation: self.span,
            branches,
            else_branch,
        })
    }
}

impl Parse for CaseBlockGroup {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = DelimitedGroup::parse(tokens, DelimiterPair::CaseEnd)?;

        let kw_span = group.open.clone();
        let end_span = group.close.clone();
        let span = group.span.clone();

        let mut group_tokens = group.into_inner_tokens();

        let cond_expr = Expr::parse(&mut group_tokens)?;
        
        let of_span = group_tokens.match_one(Keyword::Of)?.into_span();
        
        Ok(CaseBlockGroup {
            kw_span,
            of_span,
            cond_expr,
            span,
            tokens: group_tokens,
            end_span,
        })
    }
}
