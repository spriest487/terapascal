use crate::parse::Generate;
use crate::parse::LookAheadTokenStream;
use crate::parse::Match;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Operator;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
use std::fmt::Write;
use std::rc::Rc;
use std::{fmt, vec};

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash)]
pub struct Ident {
    pub name: Rc<String>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl PartialEq<String> for Ident {
    fn eq(&self, name: &String) -> bool {
        *self.name == *name
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, name: &str) -> bool {
        *self.name == name
    }
}

impl Ident {
    pub fn new(text: &str, span: impl Into<Span>) -> Self {
        Self {
            name: Rc::new(text.to_string()),
            span: span.into(),
        }
    }
    
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens
            .match_one(Matcher::AnyIdent)
            .map(|tt| tt.into_ident().unwrap())
    }
}

impl Match for Ident {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ident({})", self.name)
    }
}

#[derive(Clone, Hash)]
pub struct Path<Part> {
    parts: Vec<Part>,
}

impl<Part> Path<Part> {
    pub fn new(name: Part, namespace: impl IntoIterator<Item = Part>) -> Self {
        let mut path: Vec<_> = namespace.into_iter().collect();
        path.push(name);

        Self { parts: path }
    }

    // paths should never be empty
    pub unsafe fn empty() -> Self {
        Self { parts: Vec::new() }
    }

    pub fn from_parts(parts: impl IntoIterator<Item = Part>) -> Self {
        let parts: Vec<_> = parts.into_iter().collect();
        Self { parts }
    }

    pub fn from_vec(parts: Vec<Part>) -> Self {
        assert!(parts.len() >= 1);
        Self { parts }
    }

    pub fn into_vec(self) -> Vec<Part> {
        self.parts
    }

    pub fn iter(&self) -> impl Iterator<Item = &Part> {
        self.parts.iter()
    }

    pub fn map<IntoPart: fmt::Debug>(self, f: impl FnMut(Part) -> IntoPart) -> Path<IntoPart> {
        Path {
            parts: self.parts.into_iter().map(f).collect(),
        }
    }

    pub fn push(&mut self, part: Part) {
        self.parts.push(part);
    }

    pub fn pop(&mut self) -> Part {
        if self.len() == 1 {
            panic!("can't pop the last part from a path");
        }

        self.parts.pop().unwrap()
    }

    pub fn set(&mut self, parts: impl IntoIterator<Item = Part>) {
        self.parts.clear();
        self.parts.extend(parts);

        assert!(self.parts.len() >= 1)
    }

    pub fn extend(&mut self, parts: impl IntoIterator<Item = Part>) {
        self.parts.extend(parts)
    }

    pub fn first(&self) -> &Part {
        self.parts.first().unwrap()
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn last(&self) -> &Part {
        self.parts.last().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut Part {
        self.parts.last_mut().unwrap()
    }

    pub fn single(&self) -> &Part {
        if self.parts.len() > 1 {
            panic!("single() expects ident path to have only 1 part");
        }
        self.last()
    }

    pub fn as_slice(&self) -> &[Part] {
        self.parts.as_slice()
    }

    pub fn child(mut self, part: Part) -> Self {
        self.parts.push(part);
        self
    }
}

impl<Part: PartialEq> Path<Part> {
    pub fn is_parent_of(&self, other: &Self) -> bool {
        self.parts.len() < other.parts.len()
            && &other.parts[0..self.parts.len()] == self.parts.as_slice()
    }
}

impl<Part> IntoIterator for Path<Part> {
    type Item = Part;
    type IntoIter = vec::IntoIter<Part>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.into_iter()
    }
}

impl<OtherPart, Part> PartialEq<Path<OtherPart>> for Path<Part>
where
    Part: fmt::Debug + PartialEq<OtherPart>,
    OtherPart: fmt::Debug,
{
    fn eq(&self, other: &Path<OtherPart>) -> bool {
        self.parts == other.parts
    }
}

impl<Part: Eq + fmt::Debug> Eq for Path<Part> {
}

impl<Part: Clone> Path<Part> {
    pub fn parent(&self) -> Option<Self> {
        if self.parts.len() == 1 {
            None
        } else {
            let parent_parts = self.parts[0..self.parts.len() - 1].to_vec();
            Some(Self {
                parts: parent_parts,
            })
        }
    }
}

impl<Part: fmt::Display> Path<Part> {
    pub fn join(&self, sep: impl fmt::Display) -> String {
        let mut joined = String::new();
        for (i, part) in self.iter().enumerate() {
            if i > 0 {
                write!(joined, "{}", sep).unwrap();
            }
            write!(joined, "{}", part).unwrap()
        }
        joined
    }
}

impl<Part> From<Part> for Path<Part> {
    fn from(part: Part) -> Self {
        Self { parts: vec![part] }
    }
}

pub type IdentPath = Path<Ident>;

impl IdentPath {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let path = tokens.match_repeating(|i, tokens| {
            if i > 0 && tokens.match_one_maybe(Operator::Period).is_none() {
                return Ok(Generate::Break);
            }

            let ident_tt = tokens.match_one(Matcher::AnyIdent)?;
            let ident = ident_tt.into_ident().unwrap();
            Ok(Generate::Yield(ident))
        })?;

        assert!(
            !path.is_empty(),
            "parsed ident path must always have 1+ parts"
        );

        Ok(IdentPath::from_parts(path))
    }

    pub fn path_span(&self) -> Span {
        match self.parts.len() {
            1 => self.parts[0].span.clone(),
            
            _ => {
                // the relevant span is the most specific part of the path, e.g. the last.
                // if any previous part is directly contiguous (ending on the previous character on
                // the same line) as the part following it, include that part too
                // e.g. for A.B.C where B.C is an expression in namespace A (declared elsewhere),
                // the span of the path should be the usage of B.C.
                let mut span = self.last().span.clone();
                for part in self.parts.iter().rev().skip(1) {
                    let prev_span = &part.span;
                    if prev_span.file == span.file 
                        && prev_span.end.line == span.start.line
                        && span.start.col > 1
                        && prev_span.end.col == span.start.col - 2
                    {
                        span = prev_span.to(&span);
                    }
                }
                
                span
            }
        }
    }

    pub fn to_string_path(&self) -> Path<String> {
        Path {
            parts: self.parts.iter().map(|p| (*p.name).clone()).collect(),
        }
    }
}

impl Spanned for Path<Ident> {
    fn span(&self) -> &Span {
        self.last().span()
    }
}

pub trait PathConcat {
    fn separator() -> &'static str;
}

impl PathConcat for Ident {
    fn separator() -> &'static str {
        "."
    }
}

impl<Part: fmt::Display + PathConcat> fmt::Display for Path<Part> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sep = Part::separator();        
        write!(f, "{}", self.join(sep))
    }
}

impl<Part: fmt::Display + PathConcat> fmt::Debug for Path<Part> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IdentPath({})", self)
    }
}
