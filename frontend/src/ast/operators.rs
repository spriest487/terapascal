use crate::parse::Matchable;
use crate::parse::Matcher;
use crate::parse::SequenceMatcher;
use std::fmt;
use std::ops::Add;
use std::ops::BitOr;

#[derive(Eq, PartialEq, Clone, Debug, Copy, Hash)]
pub enum Operator {
    Assignment,
    CompoundAssignment(CompoundAssignmentOperator),

    Add,
    Sub,
    Mul,
    FDiv,
    IDiv,
    Mod,

    Caret,
    AddressOf,

    As,

    And,
    Not,
    Or,

    With,

    // no bitwise xor since the caret operator used for derefs doubles as that
    BitAnd,
    BitNot,
    BitOr,

    Equals,
    NotEquals,
    Gt,
    Gte,
    Lt,
    Lte,

    RangeInclusive,

    In,

    Period,

    Shl,
    Shr,

    // not used as a syntactical operator, but included so it can easily
    // participate in precedence comparisons
    Index,
    Call,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Period => write!(f, "."),
            Operator::Caret => write!(f, "^"),
            Operator::AddressOf => write!(f, "@"),
            
            Operator::Assignment => write!(f, ":="),
            Operator::CompoundAssignment(a) => write!(f, "{}", a),
            
            Operator::Equals => write!(f, "="),
            Operator::NotEquals => write!(f, "<>"),
            
            Operator::Shl => write!(f, "shl"),
            Operator::Shr => write!(f, "shr"),
            
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::FDiv => write!(f, "/"),
            Operator::IDiv => write!(f, "div"),
            Operator::Mod => write!(f, "mod"),
            
            Operator::And => write!(f, "and"),
            Operator::Not => write!(f, "not"),
            Operator::Or => write!(f, "or"),
            
            Operator::Gt => write!(f, ">"),
            Operator::Gte => write!(f, ">="),
            Operator::Lt => write!(f, "<"),
            Operator::Lte => write!(f, "<="),
            
            Operator::In => write!(f, "in"),
            Operator::RangeInclusive => write!(f, ".."),
            
            Operator::With => write!(f, "with"),
            
            Operator::As => write!(f, "as"),
            Operator::BitAnd => write!(f, "&"),
            Operator::BitNot => write!(f, "~"),
            Operator::BitOr => write!(f, "|"),

            Operator::Index => write!(f, "[...]"),
            Operator::Call => write!(f, "(...)"),
        }
    }
}

impl Matchable for Operator {
    fn as_matcher(&self) -> Matcher {
        Matcher::Operator(*self)
    }
}

impl<Rhs> BitOr<Rhs> for Operator 
    where Rhs: Into<Matcher>
{
    type Output = Matcher;

    fn bitor(self, rhs: Rhs) -> Matcher {
        self.as_matcher().or(rhs.into())
    }
}

impl<Rhs> Add<Rhs> for Operator
where
    Rhs: Into<Matcher>
{
    type Output = SequenceMatcher;

    fn add(self, rhs: Rhs) -> Self::Output {
        self.as_matcher() + rhs.into()
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum Position {
    Prefix,
    Postfix,
    Binary,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Position::Prefix => "prefix",
            Position::Binary => "binary",
            Position::Postfix => "postfix",
        };

        write!(f, "{}", name)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy, Hash)]
pub enum CompoundAssignmentOperator {
    AddAssign,
    SubAssign,
    MulAssign,
    FDivAssign,
}

impl CompoundAssignmentOperator {
    pub fn binary_operator(self) -> Operator {
        match self {
            CompoundAssignmentOperator::AddAssign => Operator::Add,
            CompoundAssignmentOperator::SubAssign => Operator::Sub,
            CompoundAssignmentOperator::MulAssign => Operator::Mul,
            CompoundAssignmentOperator::FDivAssign => Operator::FDiv,
        }
    }
}

impl From<CompoundAssignmentOperator> for Operator {
    fn from(op: CompoundAssignmentOperator) -> Self {
        Operator::CompoundAssignment(op)
    }
}

/// canonical operator precedence ordering. operations higher in the list
/// take precedence over ones below them
static PRECEDENCE: [(Operator, Position); 37] = [
    (Operator::Index, Position::Binary),
    (Operator::Period, Position::Binary),
    (Operator::With, Position::Postfix),
    (Operator::Call, Position::Postfix),
    (Operator::Caret, Position::Postfix),
    (Operator::AddressOf, Position::Prefix),
    (Operator::Add, Position::Prefix),
    (Operator::Sub, Position::Prefix),
    (Operator::Not, Position::Prefix),
    (Operator::BitNot, Position::Prefix),
    (Operator::As, Position::Postfix),
    (Operator::Shl, Position::Binary),
    (Operator::Shr, Position::Binary),
    (Operator::Mul, Position::Binary),
    (Operator::FDiv, Position::Binary),
    (Operator::IDiv, Position::Binary),
    (Operator::Mod, Position::Binary),
    (Operator::Add, Position::Binary),
    (Operator::Sub, Position::Binary),
    (Operator::Equals, Position::Binary),
    (Operator::NotEquals, Position::Binary),
    (Operator::Gt, Position::Binary),
    (Operator::Gte, Position::Binary),
    (Operator::Lt, Position::Binary),
    (Operator::Lte, Position::Binary),
    (Operator::In, Position::Binary),
    (Operator::BitAnd, Position::Binary),
    (Operator::And, Position::Binary),
    (Operator::BitOr, Position::Binary),
    (Operator::Caret, Position::Binary),
    (Operator::Or, Position::Binary),
    (Operator::RangeInclusive, Position::Binary),
    (Operator::Assignment, Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::AddAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::SubAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::MulAssign), Position::Binary),
    (Operator::CompoundAssignment(CompoundAssignmentOperator::FDivAssign), Position::Binary),
];

impl Operator {
    pub fn for_position(position: Position) -> impl Iterator<Item = Operator> {
        PRECEDENCE
            .iter()
            .filter_map(move |(op, pos)| if *pos == position { Some(*op) } else { None })
    }

    pub fn precedence(self, in_pos: Position) -> usize {
        match self {
            Operator::Period | Operator::Index | Operator::Call => 0,

            _ => PRECEDENCE
                .iter()
                .enumerate()
                .find(|(_, (op, pos))| *op == self && *pos == in_pos)
                .map(|(index, _)| index)
                .unwrap_or_else(|| {
                    panic!(
                        "operator {} must have a precedence value in position {}",
                        self, in_pos
                    )
                })
        }
    }

    pub fn is_keyword(self) -> bool {
        match self {
            Operator::IDiv
            | Operator::Mod
            | Operator::As
            | Operator::And
            | Operator::Not
            | Operator::Or
            | Operator::With
            | Operator::In
            | Operator::Shl
            | Operator::Shr => true,

            _ => false,
        }
    }

    pub fn is_valid_in_pos(self, in_pos: Position) -> bool {
        PRECEDENCE
            .iter()
            .any(|&(op, pos)| self == op && in_pos == pos)
    }

    fn try_parse_text_lowercase(from: &str) -> Option<Self> {
        match from {
            "or" => Some(Operator::Or),
            "and" => Some(Operator::And),
            "not" => Some(Operator::Not),
            "in" => Some(Operator::In),
            "shl" => Some(Operator::Shl),
            "shr" => Some(Operator::Shr),
            "as" => Some(Operator::As),
            "div" => Some(Operator::IDiv),
            "mod" => Some(Operator::Mod),
            "with" => Some(Operator::With),
            _ => None,
        }
    }

    // parses operators with english names (and only those operators),
    // because these names might also be valid identifiers. the tokenizer
    // already knows how to parse the ones which have names which aren't
    // valid identifiers
    pub fn try_parse_text(from: &str, case_sensitive: bool) -> Option<Operator> {
        if case_sensitive {
            Self::try_parse_text_lowercase(from)
        } else {
            Self::try_parse_text_lowercase(&from.to_ascii_lowercase())
        }
    }
}

impl fmt::Display for CompoundAssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_str = match self {
            CompoundAssignmentOperator::AddAssign => "+",
            CompoundAssignmentOperator::SubAssign => "-",
            CompoundAssignmentOperator::MulAssign => "*",
            CompoundAssignmentOperator::FDivAssign => "/",
        };
        write!(f, "{}", op_str)
    }
}
