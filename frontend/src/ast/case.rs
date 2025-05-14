#[cfg(test)]
mod test;

use crate::ast::Annotation;
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
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

pub type CaseStmt<A = Span> = CaseBlock<A, Stmt<A>>;
pub type CaseExpr<A = Span> = CaseBlock<A, Expr<A>>;

#[derive(Debug, Clone, Eq)]
pub struct CaseBlock<A: Annotation, B> {
    pub cond_expr: Box<Expr<A>>,
    pub branches: Vec<CaseBranch<A, B>>,
    pub else_branch: Option<Box<B>>,

    pub annotation: A,
}

impl<A, B> PartialEq for CaseBlock<A, B>
where
    A: Annotation,
    B: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.branches == other.branches
            && self.cond_expr == other.cond_expr
            && self.else_branch == other.else_branch
    }
}

impl<A, B> Hash for CaseBlock<A, B>
where
    A: Annotation,
    B: Hash
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.branches.hash(state);
        self.cond_expr.hash(state);
        self.else_branch.hash(state);
    }
}

impl<A, B> Spanned for CaseBlock<A, B>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A, B> fmt::Display for CaseBlock<A, B>
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
            writeln!(f, "else {}", else_branch)?;
        }

        writeln!(f, "end")
    }
}

impl CaseExpr {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut group = CaseBlockGroup::parse(tokens)?;

        let mut branches = Vec::new();
        let else_branch = Self::parse_branches(&mut group.tokens, &mut branches)?;

        Ok(CaseBlock {
            cond_expr: Box::new(group.cond_expr),
            annotation: group.span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }

    fn parse_branches(
        tokens: &mut TokenStream,
        branches: &mut Vec<CaseBranch<Span, Expr>>
    ) -> ParseResult<Option<Expr>> {
        let expect_else = loop {
            let branch = CaseBranch::parse(tokens)?;
            branches.push(branch);

            match MatchExpr::match_end_of_branches(tokens) {
                MatchBranchNextItem::Branch => continue,
                MatchBranchNextItem::ElseBranch => break true,
                MatchBranchNextItem::End => break false,
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

        let expect_else = loop {
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

                    group.tokens.finish()?;

                    let match_expr = CaseExpr {
                        cond_expr: Box::new(group.cond_expr),
                        branches: expr_branches,
                        else_branch: else_branch.map(Box::new),
                        annotation: group.span,
                    };

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
        };

        let else_branch = if expect_else {
            let else_item = Stmt::parse(&mut group.tokens)?;
            group.tokens.match_one_maybe(Separator::Semicolon);

            Some(else_item)
        } else {
            None
        };

        Ok(CaseBlock {
            cond_expr: Box::new(group.cond_expr),
            annotation: group.span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}


#[derive(Debug, Clone, Eq)]
pub struct CaseBranch<A: Annotation, Item> {
    pub case_values: Vec<Expr<A>>,
    pub item: Box<Item>,
    pub span: Span,
}

impl<A, Item> PartialEq for CaseBranch<A, Item>
where
    A: Annotation,
    Item: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.case_values == other.case_values && self.item == other.item
    }
}

impl<A, Item> Hash for CaseBranch<A, Item>
where
    A: Annotation,
    Item: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.case_values.hash(state);
        self.item.hash(state);
    }
}

impl<A, Item> Spanned for CaseBranch<A, Item>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A, Item> fmt::Display for CaseBranch<A, Item>
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

impl<Item> CaseBranch<Span, Item>
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

impl<Item> CaseBranch<Span, Item>
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
        let else_branch = self.else_branch.as_ref()?.clone().to_expr()?;

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
            cond_expr: self.cond_expr.clone(),
            branches,
            else_branch: Some(Box::new(else_branch)),
            annotation: self.annotation.clone(),
        })
    }
}

struct CaseBlockGroup {
    cond_expr: Expr,
    tokens: TokenStream,
    span: Span,
}

impl Parse for CaseBlockGroup {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = DelimitedGroup::parse(tokens, DelimiterPair::CaseEnd)?;

        let span = group.span.clone();

        let mut group_tokens = group.to_inner_tokens();

        let cond_expr = Expr::parse(&mut group_tokens)?;
        
        group_tokens.match_one(Keyword::Of)?;
        
        Ok(CaseBlockGroup {
            cond_expr,
            span,
            tokens: group_tokens,
        })
    }
}
