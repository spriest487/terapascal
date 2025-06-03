pub mod error;
mod directive;

use crate::pp::directive::Directive;
use crate::pp::directive::DirectiveParser;
use crate::pp::error::PreprocessorError;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::read_source_file;
use terapascal_common::source_map::SourceMap;
use terapascal_common::source_map::SourceMapBuilder;
use terapascal_common::source_map::SourceMapEntry;
use terapascal_common::span::*;
use terapascal_common::CompileOpts;

#[derive(Debug)]
struct SymbolCondition {
    value: bool,
    start_line: usize,
}

pub struct Preprocessor {
    directive_parser: DirectiveParser,

    condition_stack: Vec<SymbolCondition>,

    filename: Arc<PathBuf>,

    output_lines: Vec<String>,
    comment_block: Option<CommentBlock>,

    current_src_line: usize,
    last_char: char,

    opts: CompileOpts,

    source_map: SourceMapBuilder,

    warnings: Vec<PreprocessorError>,
}

struct CommentBlock {
    text: String,
    src_span: Span,

    // where this commend starts in the output text - if a block comment turns out to be a
    // directive, we remove it from the output entirely after parsing its contents, so we need
    // to remember where it starts
    output_pos: Location,
}

#[derive(Clone, Debug)]
pub struct PreprocessedUnit {
    pub filename: Arc<PathBuf>,
    pub source: String,

    pub opts: CompileOpts,

    pub source_map: SourceMap,

    pub warnings: Vec<PreprocessorError>,
}

impl Preprocessor {
    pub fn new(filename: impl Into<PathBuf>, opts: CompileOpts) -> Self {
        let filename = Arc::new(filename.into());
        Preprocessor {
            filename: filename.clone(),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            opts,

            output_lines: Vec::new(),
            comment_block: None,

            current_src_line: 0,

            last_char: ' ',

            source_map: SourceMapBuilder::new(filename),

            warnings: Vec::new(),
        }
    }

    fn current_line_span(&self, col: usize) -> Span {
        let loc = Location {
            line: self.current_src_line,
            col,
        };

        Span { file: self.filename.clone(), start: loc, end: loc }
    }

    pub fn preprocess(mut self, source: &str) -> Result<PreprocessedUnit, PreprocessorError> {
        for (line_num, line) in source.lines().enumerate() {
            self.process_line(line)?;

            self.current_src_line = line_num + 1;
        }

        if let Some(condition) = self.condition_stack.last() {
            return Err(PreprocessorError::UnterminatedCondition(Span {
                file: self.filename.clone(),
                start: Location {
                    line: condition.start_line,
                    col: 0,
                },
                end: Location {
                    line: condition.start_line,
                    col: 0,
                },
            }));
        }

        if let Some(comment) = self.comment_block {
            return Err(PreprocessorError::UnterminatedComment(comment.src_span));
        }

        Ok(PreprocessedUnit {
            filename: self.filename,
            source: self.output_lines.join("\n"),
            opts: self.opts,

            source_map: self.source_map.build(),

            warnings: self.warnings,
        })
    }

    fn process_line(&mut self, mut line: &str) -> Result<(), PreprocessorError> {
        let mut output = String::new();

        let mut current_src_mapping = None;

        // line comments never contain pp directives, just discard them
        if let Some(comment_pos) = line.find("//") {
            line = &line[0..comment_pos];
        };

        for (col, line_char) in line.chars().enumerate() {
            let src_mapping = current_src_mapping.get_or_insert_with(|| {
                SourceMapEntry {
                    src: Span {
                        file: self.filename.clone(),
                        start: Location::new(self.current_src_line, col),
                        end: Location::new(self.current_src_line, col),
                    },
                    start: Location::new(self.output_lines.len(), output.len()),
                    end: Location::new(self.output_lines.len(), output.len()),
                }
            });

            if let Some(comment_block) = &mut self.comment_block {
                // continuing an existing comment
                comment_block.text.push(line_char);
                comment_block.src_span.end = Location::new(self.current_src_line, col + 1);

                let comment_starter = comment_block.text.chars().next().unwrap();

                // did it terminate the comment this block was started with?
                let terminated = (line_char == '}' && comment_starter == '{')
                    || (line_char == ')' && self.last_char == '*' && comment_starter == '(');

                if terminated {
                    let comment_block = self.comment_block.take().unwrap();

                    // start a new mapping entry after the comment closes
                    current_src_mapping = None;

                    if line_char == '}' {
                        self.process_directive(comment_block, col, &mut output)?;
                    }
                }
            } else {
                if line_char == '{' {
                    let comment_src_span = self.current_line_span(col.saturating_sub("{".len()));

                    // start a new { } comment block
                    self.comment_block = Some(CommentBlock {
                        text: "{".to_string(),
                        src_span: comment_src_span,
                        output_pos: Location {
                            line: self.output_lines.len(),
                            col: output.len(),
                        },
                    });

                    if src_mapping.start != src_mapping.end {
                        self.source_map.add(src_mapping.start, src_mapping.end, src_mapping.src.clone());
                    }
                } else if self.last_char == '(' && line_char == '*' {
                    let comment_src_span = self.current_line_span(col.saturating_sub("(*".len()));

                    // start a new (* *) comment block
                    self.comment_block = Some(CommentBlock {
                        text: "(*".to_string(),
                        src_span: comment_src_span,
                        output_pos: Location {
                            line: self.output_lines.len(),
                            col: output.len(),
                        },
                    });
                    // it's a two character comment sequence so pop the (
                    output.pop();

                    if src_mapping.start != src_mapping.end {
                        self.source_map.add(src_mapping.start, src_mapping.end, src_mapping.src.clone());
                    }
                } else if self.condition_active() {
                    output.push(line_char);
                }

                src_mapping.src.end.col = col;
                src_mapping.end.col = output.len();
            }

            self.last_char = line_char;
        }

        if self.comment_block.is_none() {
            if let Some(src_mapping) = current_src_mapping {
                if src_mapping.start != src_mapping.end {
                    self.source_map.add(src_mapping.start, src_mapping.end, src_mapping.src);
                }
            }

            // new line at end of real line
            self.output_lines.push(output);
        }

        // if we're currently building a comment, add the newline to the comment text too
        if let Some(comment_block) = &mut self.comment_block {
            comment_block.text.push('\n');
        }

        Ok(())
    }

    // true when we haven't encountered any conditional compilation flags, or all conditional
    // compilation flags in the current stack are true
    fn condition_active(&self) -> bool {
        self.condition_stack
            .iter()
            .all(|symbol_condition| symbol_condition.value)
    }

    fn push_def_condition(&mut self, symbol: &str, positive: bool) {
        // eprintln!("{}pushing {}: {} @ {}", " ".repeat(self.condition_stack.len()), match positive { true => "ifdef", false => "ifndef" }, symbol, self.current_src_line);
        self.condition_stack.push(SymbolCondition {
            value: {
                let has_symbol = self.opts.defined(symbol);
                if positive {
                    has_symbol
                } else {
                    !has_symbol
                }
            },
            start_line: self.current_src_line,
        });
    }

    fn push_opt_condition(&mut self, opt: &str, on: bool) {
        // eprintln!("{}pushing ifopt: {}{} @ {}", " ".repeat(self.condition_stack.len()), opt, match on { true => '+', false => '-' }, self.current_src_line);
        self.condition_stack.push(SymbolCondition {
            value: on == self.opts.is_switch_on(opt),
            start_line: self.current_src_line,
        });
    }

    fn pop_condition(&mut self, col: usize) -> Result<(), PreprocessorError> {
        if self.condition_stack.pop().is_none() {
            return Err(PreprocessorError::UnexpectedEndIf(self.current_line_span(col)));
        }

        // eprintln!("{}popped condition @ {}", " ".repeat(self.condition_stack.len()), self.current_src_line);

        Ok(())
    }

    fn process_directive(&mut self,
        comment_block: CommentBlock,
        current_col: usize,
        output: &mut String
    ) -> Result<(), PreprocessorError> {
        if comment_block.text.chars().nth(1) != Some('$') {
            // no directive, just an empty brace comment
            return Ok(());
        }

        let directive_src_span = comment_block.src_span.clone();
        let directive_start_pos = comment_block.output_pos;

        // totally remove the text of the directive from the output, if it expands to some other
        // text, like an include, it needs to replace the directive comment completely
        while self.output_lines.len() > directive_start_pos.line {
            *output = self.output_lines.pop().unwrap();
        }
        output.truncate(directive_start_pos.col);

        // skip 2 for `{$` and take 1 less for the closing `}`
        let directives_text = comment_block
            .text
            .chars()
            .skip(2)
            .take(comment_block.text.len() - 3)
            .collect::<String>();

        let directives_items = directives_text
            .split(',')
            .map(|s| s.trim());

        for directive_text in directives_items {
            match self.directive_parser.parse(&directive_text) {
                Some(Directive::Define(symbol)) => {
                    if self.condition_active() {
                        self.opts.define(symbol);
                    }
                }

                Some(Directive::Undef(symbol)) => {
                    if !self.condition_active() {
                        continue;
                    }

                    self.opts.undef(&symbol);
                }

                Some(Directive::IfDef(symbol)) => {
                    self.push_def_condition(&symbol, true);
                },

                Some(Directive::IfOpt(switch, on)) => {
                    self.push_opt_condition(&switch, on);
                }

                Some(Directive::IfNDef(symbol)) => {
                    self.push_def_condition(&symbol, false);
                },

                Some(Directive::Else) => match self.condition_stack.last_mut() {
                    None => {
                        return Err(PreprocessorError::UnexpectedEndIf(directive_src_span.clone()));
                    },

                    Some(condition) => {
                        condition.value = !condition.value;
                    }
                },

                Some(Directive::ElseIf(symbol)) => {
                    self.pop_condition(current_col)?;
                    self.push_def_condition(&symbol, true);
                }

                Some(Directive::EndIf { .. }) => {
                    self.pop_condition(current_col)?;
                },

                Some(Directive::Switch(switch, on)) => {
                    if self.condition_active() {
                        self.opts.set_switch(&switch, on);
                    }
                }

                Some(Directive::LinkLib(lib_name)) => {
                    if self.condition_active() {
                        self.opts.link_lib(lib_name.clone());
                    }
                }

                Some(Directive::Mode(mode)) => {
                    if self.condition_active() {
                        self.opts.lang_mode = mode;
                    }
                }

                Some(Directive::Include(filename)) => {
                    let full_path = match self.filename.parent() {
                        Some(parent) => parent.join(&filename),
                        None => PathBuf::from(&filename),
                    };

                    let include_src = read_source_file(&full_path)
                        .map_err(|err| PreprocessorError::IncludeError {
                            filename,
                            err: err.to_string(),
                            at: directive_src_span.clone(),
                        })?;

                    self.include_file(full_path, include_src, output)?;
                }

                None => {
                    if !self.condition_active() {
                        continue;
                    }

                    let err = PreprocessorError::IllegalDirective {
                        directive: directive_text.to_string(),
                        at: directive_src_span.clone(),
                    };

                    if !self.opts.strict_switches() {
                        self.warnings.push(err);
                    } else {
                        return Err(err);
                    }
                }
            }
        }

        Ok(())
    }

    fn include_file(&mut self, full_path: PathBuf, include_src: String, output: &mut String) -> Result<(), PreprocessorError> {
        let pp = Preprocessor::new(full_path, self.opts.clone());
        let include_output = pp.preprocess(&include_src)?;

        self.warnings.extend(include_output.warnings);

        if !include_output.source.is_empty() {
            let line_offset = self.output_lines.len();
            let col_offset = output.len();

            // this excludes the global entry which is mapped to 0:0 -> 0:0
            let mapping_entries = include_output.source_map.into_iter().filter(|e| e.start != e.end);
            for mapping in mapping_entries {
                let mut start = mapping.start;
                let mut end = mapping.end;

                start.line += line_offset;
                end.line += line_offset;

                // entries on the first line of the included file need to be offset since they're
                // being pasted in at the current location (subsequent lines don't include any offset)
                if mapping.start.line == 0 {
                    start.col += col_offset;
                }
                if mapping.end.line == 0 {
                    end.col += col_offset;
                }

                self.source_map.add(start, end, mapping.src);
            }

            for include_source_line in include_output.source.lines() {
                output.push_str(include_source_line);

                self.output_lines.push(output.clone());
                output.clear();
            }

            // actually we're still editing the last line
            *output = self.output_lines.pop().unwrap();
        }

        Ok(())
    }
}
