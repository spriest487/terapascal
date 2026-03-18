use serde::Deserialize;
use serde::Serialize;
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize, Hash, Eq, PartialEq, Ord)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Version {
            major,
            minor,
            patch,
        }
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let order = self.major.cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch));

        Some(order)
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Error)]
pub enum VersionParseError {
    #[error("Invalid format")]
    InvalidFormat,
}

impl FromStr for Version {
    type Err = VersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut major = 0;
        let mut minor = 0;
        let mut patch = 0;

        let mut part_num = 0;

        for c in s.chars() {
            if let Some(digit) = c.to_digit(10) {
                let part = match part_num {
                    0 => &mut major,
                    1 => &mut minor,
                    _ => &mut patch,
                };

                if digit == 0 && *part == 0 {
                    continue;
                }

                *part *= 10;
                *part += digit;
            } else if c == '.' {
                if part_num >= 2 {
                    return Err(VersionParseError::InvalidFormat);
                }

                part_num += 1;
            } else {
                return Err(VersionParseError::InvalidFormat)
            }
        }

        Ok(Version {
            major,
            minor,
            patch,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::version::{Version, VersionParseError};
    use std::str::FromStr;

    #[test]
    pub fn parses_major() {
        assert_eq!(Ok(Version::new(1, 0, 0)), Version::from_str("1"))
    }

    #[test]
    pub fn parses_major_and_minor() {
        assert_eq!(Ok(Version::new(1, 2, 0)), Version::from_str("1.2"))
    }

    #[test]
    pub fn parses_major_and_minor_and_patch() {
        assert_eq!(Ok(Version::new(1, 2, 3)), Version::from_str("1.2.3"))
    }

    #[test]
    pub fn parses_with_multiple_chars() {
        assert_eq!(Ok(Version::new(999, 999, 999)), Version::from_str("999.999.999"))
    }

    #[test]
    pub fn parses_with_leading_zeroes() {
        assert_eq!(Ok(Version::new(100, 10, 1)), Version::from_str("00100.00010.01"))
    }

    #[test]
    pub fn fails_with_other_chars() {
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str("A"))
    }

    #[test]
    pub fn fails_with_other_trailing_chars() {
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str("1.2.3A"));
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str("1.2A.3"));
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str("1A.2.3"));
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str(" 1.2.3"));
        assert_eq!(Err(VersionParseError::InvalidFormat), Version::from_str("1.2.3 "));
    }
}