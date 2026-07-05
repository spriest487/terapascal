use crate::path::Path;
use crate::path::PathConcat;
use crate::span::Span;
use crate::span::Spanned;
use derivative::Derivative;
use std::fmt;
use std::str::FromStr;
use std::sync::Arc;

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash)]
pub struct Ident {
    pub name: Arc<String>,

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
            name: Arc::new(text.to_string()),
            span: span.into(),
        }
    }
    
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
    
    pub fn with_span(self, span: Span) -> Self {
        Self {
            name: self.name.clone(),
            span,
        }
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

impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

pub type IdentPath = Path<Ident>;

impl IdentPath {
    pub fn path_span(&self) -> Span {
        match self.len() {
            1 => self.as_slice()[0].span.clone(),
            
            _ => {
                // the relevant span is the most specific part of the path, e.g. the last.
                // if any previous part is directly contiguous (ending on the previous character on
                // the same line) as the part following it, include that part too
                // e.g. for A.B.C where B.C is an expression in namespace A (declared elsewhere),
                // the span of the path should be the usage of B.C.
                let mut span = self.last().span.clone();
                for part in self.iter().rev().skip(1) {
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
        Path::from_parts(self.iter().map(|p| (*p.name).clone()))
    }
}

impl Spanned for Path<Ident> {
    fn span(&self) -> &Span {
        self.last().span()
    }
}

impl PathConcat for Ident {
    fn separator() -> &'static str {
        "."
    }
}

impl<Part> FromStr for Path<Part> where Part: FromStr {
    type Err = Part::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = Vec::with_capacity(1);
        for part_str in s.split('.') {
            let part = Part::from_str(part_str)?;
            parts.push(part);
        }

        Ok(Self::from_parts(parts))
    }
}
