#[cfg(test)]
mod test;

use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::parse::MatchOneOf;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
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

impl CaseBlock<Span, Expr> {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = DelimitedGroup::parse(tokens, DelimiterPair::CaseEnd)?;

        let case_kw = group.open.clone();
        let end_kw = group.close.clone();
        let mut group_tokens = group.to_inner_tokens();

        let result = Self::parse_group(&mut group_tokens, case_kw, end_kw)?;

        group_tokens.finish()?;

        Ok(result)
    }

    fn parse_group(tokens: &mut TokenStream, case_kw: Span, end_kw: Span) -> ParseResult<Self> {
        let cond_expr = Expr::parse(tokens)?;

        tokens.match_one(Keyword::Of)?;
        let mut branches = Vec::new();

        // is there a valid separator between the last and current statements?
        let mut prev_sep = true;

        let else_branch = loop {
            if tokens.look_ahead().next().is_none() {
                break None;
            }

            if branches.len() > 0 {
                if let Some(..) = tokens.match_one_maybe(Keyword::Else) {
                    let else_item = Expr::parse(tokens)?;

                    // allow a semicolon separator between the "else" stmt and the end keyword
                    tokens.match_one_maybe(Separator::Semicolon);

                    break Some(else_item);
                }
            } else if !prev_sep {
                Self::expect_end(&branches, tokens)?;
            }

            let case_branch = CaseBranch::parse(tokens)?;
            branches.push(case_branch);

            // a semicolon is required to separate branches, but not before the final "end"
            // or "else" keywords. if a stmt isn't followed by the separator, it must be the
            // last one, and if not we'll get a parse error
            prev_sep = tokens.match_one_maybe(Separator::Semicolon).is_some();
        };

        let span = case_kw.to(&end_kw);

        Ok(CaseBlock {
            cond_expr: Box::new(cond_expr),
            annotation: span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}

impl CaseBlock<Span, Stmt> {
    pub fn parse_stmt(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = DelimitedGroup::parse(tokens, DelimiterPair::CaseEnd)?;

        let case_kw = group.open.clone();
        let end_kw = group.close.clone();
        let mut group_tokens = group.to_inner_tokens();

        let result = Self::parse_group(&mut group_tokens, case_kw, end_kw)?;

        group_tokens.finish()?;

        Ok(result)
    }

    fn parse_group(tokens: &mut TokenStream, case_kw: Span, end_kw: Span) -> ParseResult<Self> {
        let cond_expr = Expr::parse(tokens)?;

        tokens.match_one(Keyword::Of)?;
        let mut branches = Vec::new();

        // is there a valid separator between the last and current statements?
        let mut prev_sep = true;

        let else_branch = loop {
            if tokens.look_ahead().next().is_none() {
                break None;
            }

            if branches.len() > 0 {
                if let Some(..) = tokens.match_one_maybe(Keyword::Else) {
                    let else_item = Stmt::parse(tokens)?;

                    // allow a semicolon separator between the "else" stmt and the end keyword
                    tokens.match_one_maybe(Separator::Semicolon);

                    break Some(else_item);
                }
            } else if !prev_sep {
                Self::expect_end(&branches, tokens)?;
            }

            let case_branch = CaseBranch::parse(tokens)?;
            branches.push(case_branch);

            // a semicolon is required to separate branches, but not before the final "end"
            // or "else" keywords. if a stmt isn't followed by the separator, it must be the
            // last one, and if not we'll get a parse error
            prev_sep = tokens.match_one_maybe(Separator::Semicolon).is_some();
        };

        let span = case_kw.to(&end_kw);

        Ok(CaseBlock {
            cond_expr: Box::new(cond_expr),
            annotation: span,
            branches,
            else_branch: else_branch.map(Box::new),
        })
    }
}

impl<B> CaseBlock<Span, B>
where
    B: Parse + Spanned,
{
    fn expect_end(branches: &[CaseBranch<Span, B>], tokens: &mut TokenStream) -> ParseResult<()> {
        // this match will fail - there was no separator after the last branch,
        // so we expected the end
        if branches.len() > 0 {
            tokens.match_one(Keyword::End.or(Keyword::Else))?;
        } else {
            tokens.match_one(Keyword::End)?;
        };

        unreachable!("previous match will always fail");
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
    Item: Parse + Spanned,
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut values = Vec::new();
        loop {
            let value = Expr::parse(tokens)?;
            values.push(value);

            if tokens.match_one_maybe(Separator::Comma).is_none() {
                break;
            }
        }
        
        let span = values[0].span().to(values[values.len() - 1].span());

        tokens.match_one(Separator::Colon)?;
        let item = Item::parse(tokens)?;

        Ok(CaseBranch {
            case_values: values,
            item: Box::new(item),
            span,
        })
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
