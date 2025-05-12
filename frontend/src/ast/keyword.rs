use crate::parse::{Matchable, Matcher, SequenceMatcher};
use std::ops::Add;
use std::ops::BitOr;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Keyword {
    Var,

    Function,
    Procedure,
    Lambda,
    Constructor,
    Destructor,

    Begin,
    End,
    Unsafe,

    Uses,

    Type,
    Record,
    Packed,
    Class,
    Variant,
    Set,

    Case,
    Match,
    If,
    While,
    Then,
    Else,
    For,
    To,
    DownTo,
    Do,
    Break,
    Continue,

    Nil,

    Program,
    Library,
    Unit,
    Interface,
    Implementation,
    Initialization,
    Finalization,
    
    Array,
    Of,
    Const,
    Weak,

    Out,
    
    True,
    False,

    Try,
    Except,
    Finally,
    Raise,

    Exit,
    Is,
    
    Where,

    SizeOf,
    Default,
    TypeInfo,

    Public,
    Private,
    Published,
}

impl Keyword {
    fn try_parse_lowercase(from: &str) -> Option<Self> {
        match from {
            "var" => Some(Keyword::Var),
            
            "function" => Some(Keyword::Function),
            "procedure" => Some(Keyword::Procedure),
            "lambda" => Some(Keyword::Lambda),
            "constructor" => Some(Keyword::Constructor),
            "destructor" => Some(Keyword::Destructor),
            
            "begin" => Some(Keyword::Begin),
            "end" => Some(Keyword::End),
            "unsafe" => Some(Keyword::Unsafe),
            
            "uses" => Some(Keyword::Uses),
            
            "type" => Some(Keyword::Type),
            "class" => Some(Keyword::Class),
            "record" => Some(Keyword::Record),
            "packed" => Some(Keyword::Packed),
            "variant" => Some(Keyword::Variant),
            "set" => Some(Keyword::Set),
            
            "case" => Some(Keyword::Case),
            "match" => Some(Keyword::Match),
            "if" => Some(Keyword::If),
            "while" => Some(Keyword::While),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "nil" => Some(Keyword::Nil),
            "for" => Some(Keyword::For),
            "to" => Some(Keyword::To),
            "downto" => Some(Keyword::DownTo),
            "do" => Some(Keyword::Do),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            
            "program" => Some(Keyword::Program),
            "library" => Some(Keyword::Library),
            "unit" => Some(Keyword::Unit),
            "interface" => Some(Keyword::Interface),
            "implementation" => Some(Keyword::Implementation),
            "initialization" => Some(Keyword::Initialization),
            "finalization" => Some(Keyword::Finalization),

            "array" => Some(Keyword::Array),
            "of" => Some(Keyword::Of),
            "const" => Some(Keyword::Const),
            "weak" => Some(Keyword::Weak),

            "out" => Some(Keyword::Out),

            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),

            "try" => Some(Keyword::Try),
            "except" => Some(Keyword::Except),
            "finally" => Some(Keyword::Finally),
            "raise" => Some(Keyword::Raise),
            "exit" => Some(Keyword::Exit),
            
            "sizeof" => Some(Keyword::SizeOf),
            "default" => Some(Keyword::Default),
            "typeinfo" => Some(Keyword::TypeInfo),
            
            "is" => Some(Keyword::Is),
            "where" => Some(Keyword::Where),
            
            "public" => Some(Keyword::Public),
            "private" => Some(Keyword::Private),
            "published" => Some(Keyword::Published),

            _ => None,
        }
    }

    pub fn try_parse(from: &str, case_sensitive: bool) -> Option<Self> {
        if case_sensitive {
            Self::try_parse_lowercase(from)
        } else {
            Self::try_parse_lowercase(&from.to_ascii_lowercase())
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Keyword::Var => "var",
            
            Keyword::Function => "function",
            Keyword::Procedure => "procedure",
            Keyword::Lambda => "lambda",
            Keyword::Constructor => "constructor",
            Keyword::Destructor => "destructor",

            Keyword::Begin => "begin",
            Keyword::End => "end",
            Keyword::Unsafe => "unsafe",
            
            Keyword::Uses => "uses",
            
            Keyword::Type => "type",
            Keyword::Class => "class",
            Keyword::Record => "record",
            Keyword::Packed => "packed",
            Keyword::Variant => "variant",
            Keyword::Set => "set",
            
            Keyword::If => "if",
            Keyword::Case => "case",
            Keyword::Match => "match",
            Keyword::While => "while",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Nil => "nil",
            Keyword::For => "for",
            Keyword::To => "to",
            Keyword::DownTo => "downto",
            Keyword::Do => "do",
            Keyword::Break => "break",
            Keyword::Continue => "continue",

            Keyword::Program => "program",
            Keyword::Library => "library",
            Keyword::Unit => "unit",
            
            Keyword::Interface => "interface",
            Keyword::Implementation => "implementation",
            Keyword::Initialization => "initialization",
            Keyword::Finalization => "finalization",

            Keyword::Array => "array",
            Keyword::Of => "of",
            Keyword::Const => "const",
            Keyword::Weak => "weak",
            
            Keyword::Out => "out",
            
            Keyword::True => "true",
            Keyword::False => "false",
            
            Keyword::Try => "try",
            Keyword::Except => "except",
            Keyword::Finally => "finally",
            Keyword::Raise => "raise",
            Keyword::Exit => "exit",

            Keyword::Is => "is",
            Keyword::Where => "where",

            Keyword::SizeOf => "sizeof",
            Keyword::Default => "default",
            Keyword::TypeInfo => "typeinfo",
            
            Keyword::Public => "public",
            Keyword::Private => "private",
            Keyword::Published => "published",
        })
    }
}

impl Matchable for Keyword {
    fn as_matcher(&self) -> Matcher {
        Matcher::Keyword(*self)
    }
}

impl<Rhs> BitOr<Rhs> for Keyword
where 
    Rhs: Into<Matcher>
{
    type Output = Matcher;

    fn bitor(self, rhs: Rhs) -> Self::Output {
        self.as_matcher() | rhs.into()
    }
}

impl<Rhs> Add<Rhs> for Keyword
where
    Rhs: Into<Matcher>
{
    type Output = SequenceMatcher;

    fn add(self, rhs: Rhs) -> Self::Output {
        self.as_matcher() + rhs.into()
    }
}
