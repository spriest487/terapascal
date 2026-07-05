use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::fmt::Write as _;
use std::vec;

#[derive(Debug, Clone, Hash, Serialize, Deserialize)]
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

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Part> {
        self.parts.iter()
    }

    pub fn map<IntoPart: fmt::Debug>(&self, f: impl FnMut(&Part) -> IntoPart) -> Path<IntoPart> {
        Path {
            parts: self.parts.iter().map(f).collect(),
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

    pub fn as_mut_slice(&mut self) -> &mut [Part] {
        self.parts.as_mut_slice()
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

impl<Part> FromIterator<Part> for Path<Part> {
    fn from_iter<T: IntoIterator<Item=Part>>(iter: T) -> Self {
        Self::from_parts(iter)
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

impl<Part: fmt::Display> fmt::Display for Path<Part> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.join('.'))
    }
}