#[cfg(test)]
mod test;

use std::{path::PathBuf, rc::Rc, vec};
use crate::{Location, Span};

pub struct SourceMapBuilder {
    source_map: SourceMap,
}

impl SourceMapBuilder {
    pub fn new(src_filename: Rc<PathBuf>) -> Self {
        Self {
            source_map: SourceMap {
                entries: vec![
                    SourceMapEntry {
                        start: Location::zero(),
                        end: Location::zero(),
                        src: Span {
                            file: src_filename,
                            start: Location::zero(),
                            end: Location::zero(),
                        }
                    }
                ]
            }
        }
    }

    pub fn add(&mut self, start: Location, end: Location, src: Span) {
        self.source_map.entries.push(SourceMapEntry {
            start,
            end,
            src,
        })
    }

    pub fn build(mut self) -> SourceMap {
        self.source_map.entries.sort_by_key(|e| e.start);

        self.source_map
    }
}

#[derive(Debug, Clone)]
pub struct SourceMap {
    entries: Vec<SourceMapEntry>,
}

impl SourceMap {
    pub fn lookup(&self, start: Location, end: Location) -> Span {
        let mut closest_entry = self.entries[0].clone();

        for entry in self.entries.iter() {
            if entry.start <= start && entry.end >= end {
                closest_entry = entry.clone();
            }
        }

        let mut result = closest_entry.src;

        if start.col > closest_entry.start.col {
            result.start.col += start.col - closest_entry.start.col;
        }

        if result.end.line == result.start.line {
            result.end.col = result.start.col + (end.col.saturating_sub(start.col));
        }
        result.end.line = result.start.line + (end.line.saturating_sub(start.line));

        result
    }
}

impl IntoIterator for SourceMap {
    type Item = SourceMapEntry;
    type IntoIter = vec::IntoIter<SourceMapEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct SourceMapEntry {
    // range in the processed text
    pub start: Location,
    pub end: Location,

    // span in the original source file
    pub src: Span,
}
