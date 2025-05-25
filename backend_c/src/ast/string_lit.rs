use std::fmt;

#[derive(Debug, Clone)]
pub struct StringLiteral(pub String);

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(unsigned char*) \"")?;

        for c in self.0.chars() {
            write!(f, "{}", c.escape_default())?;
        }

        write!(f, "\"")
    }
}

impl StringLiteral {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for StringLiteral {
    fn from(s: String) -> Self {
        StringLiteral(s)
    }
}
