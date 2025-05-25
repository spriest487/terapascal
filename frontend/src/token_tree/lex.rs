use crate::consts::IntConstant;
use crate::consts::RealConstant;
use crate::token_tree::DelimitedGroup;
use crate::token_tree::DelimiterPair;
use crate::token_tree::Separator;
use crate::token_tree::TokenTree;
use crate::token_tree::TokenizeError;
use crate::token_tree::TokenizeResult;
use terapascal_common::span::*;
use terapascal_common::TracedError;
use crate::pp::PreprocessedUnit;
use std::rc::Rc;
use terapascal_common::source_map::SourceMap;
use crate::ast::Ident;
use crate::ast::keyword::Keyword;
use crate::ast::operators::CompoundAssignmentOperator;
use crate::ast::operators::Operator;

pub fn lex(
    unit: PreprocessedUnit,
) -> TokenizeResult<Vec<TokenTree>> {
    let mut lexer = Lexer {
        source_map: unit.source_map,

        case_sensitive: unit.opts.case_sensitive,

        line: Vec::new(),
        location: Location { line: 0, col: 0 },

        line_buf: Vec::new(),
        delim_stack: Vec::new(),
    };

    let mut tokens = Vec::new();
    for (line_num, line) in unit.source.lines().enumerate() {
        lexer.line = line.chars().collect();
        lexer.location = Location {
            line: line_num,
            col: 0,
        };

        loop {
            if !lexer.next_token()? {
                tokens.extend(lexer.line_buf.drain(0..));
                break;
            }
        }
    }

    if let Some(unmatched) = lexer.delim_stack.pop() {
        return Err(TracedError::trace(TokenizeError::UnmatchedDelimiter {
            span: lexer.src_span_from(unmatched.open.start),
            to_match: unmatched.open,
            delim: unmatched.delim,
        }));
    }

    Ok(tokens)
}

struct DelimGroupBuilder {
    delim: DelimiterPair,
    open: Span,
    inner: Vec<TokenTree>,
}

struct Lexer {
    line: Vec<char>,
    location: Location,

    case_sensitive: bool,

    source_map: SourceMap,

    line_buf: Vec<TokenTree>,
    delim_stack: Vec<DelimGroupBuilder>,
}

fn find_ahead(s: &[char], predicate: impl Fn(char) -> bool) -> Option<usize> {
    s.iter()
        .enumerate()
        .filter_map(|(col, c)| if predicate(*c) { Some(col) } else { None })
        .next()
}

impl Lexer {
    // lookup the source span for the given range
    fn src_span(&self, start: Location, end: Location) -> Span {
        self.source_map.lookup(start, end)
    }

    // lookup the source span from `start` to the current position
    fn src_span_from(&self, start: Location) -> Span {
        self.src_span(start, self.current_loc())
    }

    fn current_loc(&self) -> Location {
        Location {
            line: self.location.line,
            col: if self.location.col == 0 {
                0
            } else {
                self.location.col - 1
            },
        }
    }

    fn make_float_token(&mut self, len: Option<usize>) -> TokenizeResult<TokenTree> {
        let float_chars = match len {
            None => &self.line[self.location.col..],
            Some(len) => &self.line[self.location.col..self.location.col + len],
        };

        let start_loc = self.location;

        self.location.col += float_chars.len();

        let span = self.src_span_from(start_loc);

        let float_str = float_chars.iter().collect::<String>();

        RealConstant::parse_str(&float_str)
            .map(|value| TokenTree::RealNumber {
                value,
                span: span.clone(),
            })
            .ok_or_else(|| TracedError::trace(TokenizeError::IllegalToken(float_str, span)))
    }

    fn literal_float(&mut self) -> TokenizeResult<TokenTree> {
        let find_non_num = |c| match c {
            '0'..='9' => false,
            _ => true,
        };

        // find end of integer part, as offset from self.location.col
        let mut cur = match find_ahead(&self.line[self.location.col..], find_non_num) {
            Some(after_int_part) => after_int_part,
            // no decimal point or exponent, just numbers until the end
            None => return self.make_float_token(None),
        };

        if self.line[self.location.col + cur] == '.' {
            cur += 1;
            // find end of fractional part
            cur = match find_ahead(&self.line[self.location.col + cur..], find_non_num) {
                Some(after_frac_part) => cur + after_frac_part,
                // no exponent and entire rest of line after decimal point is fractional part
                None => return self.make_float_token(None),
            }
        }

        if self.line[self.location.col + cur] == 'e' || self.line[self.location.col + cur] == 'E' {
            cur += 1;
            // find end of exponent
            cur = match find_ahead(&self.line[self.location.col + cur..], find_non_num) {
                Some(after_exponent) => cur + after_exponent,
                // exponent is last thing on this line
                None => return self.make_float_token(None),
            }
        }

        self.make_float_token(Some(cur))
    }

    fn literal_hex(&mut self) -> TokenizeResult<TokenTree> {
        let start_loc = Location {
            line: self.location.line,
            col: self.location.col, // include $ sigil
        };

        let line_after_sigil = &self.line[self.location.col + 1..];
        let next_non_hex = find_ahead(line_after_sigil, |c| match c {
            'a'..='f' | 'A'..='F' | '0'..='9' => false,
            _ => true,
        });

        // this needs to include the $ because IntConstant::parse expects it
        let int_chars = match next_non_hex {
            Some(end) => &self.line[self.location.col..=self.location.col + end],
            None => &self.line[self.location.col..],
        };

        self.location.col += int_chars.len();
        let span = self.src_span_from(start_loc);

        let int_str = int_chars.iter().collect::<String>();

        match IntConstant::parse_str(&int_str) {
            Some(value) => {
                let int_token = TokenTree::IntNumber { value, span };
                Ok(int_token)
            }
            None => Err(TracedError::trace(TokenizeError::IllegalToken(int_str, span))),
        }
    }

    fn literal_int(&mut self, sigil: bool) -> TokenizeResult<TokenTree> {
        let sigil_len = if sigil { 1 } else { 0 };
        let num_start = self.location.col + sigil_len;

        let start_loc = self.location;

        let next_non_num = {
            let from_start = &self.line[num_start..];
            find_ahead(from_start, |c| match c {
                '0'..='9' => false,
                _ => true,
            })
        };

        if !sigil {
            if let Some(num_end) = next_non_num {
                let end_char = self.line[num_start + num_end];

                if end_char == 'E' || end_char == 'e' {
                    // the only valid option for num + e is a float with an exponent
                    return self.literal_float();
                }

                if end_char == '.' && self.line.get(num_start + num_end + 1) != Some(&'.') {
                    // a period might mean this is a decimal, but two periods means this
                    // is an integer in a range
                    return self.literal_float();
                }
            }
        }

        let int_chars = match next_non_num {
            Some(end) => &self.line[self.location.col..self.location.col + end + sigil_len],
            None => &self.line[self.location.col..],
        };

        self.location.col += int_chars.len();
        let span = self.src_span_from(start_loc);

        let int_string = int_chars.iter().collect::<String>();

        match IntConstant::parse_str(&int_string) {
            Some(value) => {
                let int_token = TokenTree::IntNumber { value, span };
                Ok(int_token)
            }
            None => Err(TracedError::trace(TokenizeError::IllegalToken(int_string, span))),
        }
    }

    fn literal_string(&mut self) -> TokenizeResult<TokenTree> {
        let mut contents = String::new();

        let start_loc = self.location;

        self.location.col += 1;

        'whole_string: loop {
            match self.line.get(self.location.col) {
                // todo: better error for unterminated string literal
                None => {
                    let span = self.src_span_from(start_loc);
                    return Err(TracedError::trace(TokenizeError::UnterminatedStringLiteral(span)));
                }

                // depends on the token after this one
                Some('\'') => match self.line.get(self.location.col + 1) {
                    // it's quoted quote, add it to the contents and advance an extra col
                    Some('\'') => {
                        contents.push('\'');
                        self.location.col += 2;
                    }

                    // this string part ends but is followed by a char literal (e.g. `'hello'#10`)
                    Some('#') => {
                        self.location.col += 1;

                        'char_codes: loop {
                            let char_start_loc = self.location;
                            let char_token = self.literal_int(true)?;
                            match char_token.as_literal_int().and_then(|i| i.as_u32()).and_then(char::from_u32) {
                                Some(char_val) => {
                                    contents.push(char_val);
                                }

                                None => {
                                    let err_span = self.src_span_from(char_start_loc);
                                    return Err(TracedError::trace(TokenizeError::IllegalChar(err_span)));
                                }
                            }

                            match self.line.get(self.location.col) {
                                Some('#') => {
                                    // another char follows this one
                                },
                                Some('\'') => {
                                    // the string resumes
                                    self.location.col += 1;
                                    break 'char_codes;
                                }
                                _ => break 'whole_string,
                            }
                        }
                    }

                    // it's something else, this string ends here
                    _ => {
                        self.location.col += 1;
                        break 'whole_string;
                    },
                },

                Some(c) => {
                    contents.push(*c);
                    self.location.col += 1;
                },
            }
        }

        let span = self.src_span_from(start_loc);

        let str_token = TokenTree::String {
            value: Rc::new(contents),
            span,
        };

        Ok(str_token)
    }

    fn word(&mut self) -> TokenizeResult<TokenTree> {
        let mut token_str = String::new();

        let start_loc = self.location;

        let mut next_col = self.location.col;
        while let Some(c) = self.line.get(next_col) {
            match c {
                '0'..='9' => {
                    if !token_str.is_empty() {
                        token_str.push(*c);
                    } else {
                        // can't start with a number
                        return Err(TracedError::trace(TokenizeError::IllegalToken(c.to_string(), self.src_span_from(start_loc))));
                    }
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    token_str.push(*c);
                }

                _ => break,
            }

            next_col += 1;
        }

        self.location.col = next_col;
        let span = self.src_span_from(start_loc);

        let token = if let Some(kw) = Keyword::try_parse(&token_str, self.case_sensitive) {
            TokenTree::Keyword { kw, span }
        } else if let Some(op) = Operator::try_parse_text(&token_str, self.case_sensitive) {
            TokenTree::Operator { op, span }
        } else {
            TokenTree::Ident(Ident::new(&token_str, span))
        };

        Ok(token)
    }

    fn operator_token(&mut self, op: impl Into<Operator>, len: usize) -> TokenTree {
        let start_loc = self.location;
        self.location.col += len;
        let span = self.src_span_from(start_loc);

        TokenTree::Operator { op: op.into(), span }
    }

    fn operator_or_compound_assignment_token(&mut self, op: impl Into<Operator>, compound_op: impl Into<CompoundAssignmentOperator>) -> TokenTree {
        match self.line.get(self.location.col + 1) {
            Some('=') => self.operator_token(Operator::CompoundAssignment(compound_op.into()), 2),
            _ => self.operator_token(op.into(), 1),
        }
    }

    fn separator_token(&mut self, sep: Separator, len: usize) -> TokenTree {
        let start_loc = self.location;
        self.location.col += len;
        let span = self.src_span_from(start_loc);

        TokenTree::Separator { sep, span }
    }

    fn begin_delim_group(&mut self, delim: DelimiterPair, consume: usize) {
        self.location.col += consume;

        let (open_token, _close_token) = delim.tokens();

        // after passing `consume` chars we should be at the end of the open delim
        let open_span = self.src_span_from(Location {
            col: self.location.col - open_token.len(),
            line: self.location.line,
        });

        self.delim_stack.push(DelimGroupBuilder {
            open: open_span.clone(),
            delim,
            inner: Vec::new(),
        });
    }

    fn end_delim_group(
        &mut self,
        delim: DelimiterPair,
        consume: usize,
    ) -> TokenizeResult<TokenTree> {
        let start_loc = self.location;
        self.location.col += consume;

        let (_, close_token) = delim.tokens();
        let close_span = self.src_span_from(Location {
            line: start_loc.line,
            col: if start_loc.col < close_token.len() {
                0
            } else {
                start_loc.col - close_token.len()
            },
        });

        match self.delim_stack.pop() {
            // no group was started, or a delimited group was started, but it wasn't the
            // same type as the one we're closing
            None => Err(TracedError::trace(
                TokenizeError::UnexpectedCloseDelimited {
                    delim,
                    span: close_span,
                },
            )),

            Some(ref group) if group.delim != delim => Err(TracedError::trace(
                TokenizeError::UnexpectedCloseDelimited {
                    delim,
                    span: close_span,
                },
            )),

            Some(group) => Ok(TokenTree::Delimited(DelimitedGroup {
                delim,
                inner: group.inner,
                span: group.open.to(&close_span),
                open: group.open,
                close: close_span,
            })),
        }
    }

    fn next_token(&mut self) -> TokenizeResult<bool> {
        let next = loop {
            let next = match self.line.get(self.location.col) {
                Some(c) => *c,

                // end of stream
                None => return Ok(false),
            };

            // skip chars until we find something other than whitespace
            if !next.is_ascii_whitespace() {
                break next;
            } else {
                self.location.col += 1;
            }
        };

        let token = match next {
            '(' => {
                self.begin_delim_group(DelimiterPair::Bracket, 1);
                None
            }
            ')' => Some(self.end_delim_group(DelimiterPair::Bracket, 1)?),
            '[' => {
                self.begin_delim_group(DelimiterPair::SquareBracket, 1);
                None
            }
            ']' => Some(self.end_delim_group(DelimiterPair::SquareBracket, 1)?),

            '+' => Some(self.operator_or_compound_assignment_token(Operator::Add, CompoundAssignmentOperator::AddAssign)),
            '-' => Some(self.operator_or_compound_assignment_token(Operator::Sub, CompoundAssignmentOperator::SubAssign)),
            '*' => Some(self.operator_or_compound_assignment_token(Operator::Mul, CompoundAssignmentOperator::MulAssign)),
            '/' => Some(self.operator_or_compound_assignment_token(Operator::FDiv, CompoundAssignmentOperator::FDivAssign)),
            '|' => Some(self.operator_token(Operator::BitOr, 1)),
            '&' => Some(self.operator_token(Operator::BitAnd, 1)),
            '~' => Some(self.operator_token(Operator::BitNot, 1)),
            '@' => Some(self.operator_token(Operator::AddressOf, 1)),
            ',' => Some(self.separator_token(Separator::Comma, 1)),
            '.' => Some(match self.line.get(self.location.col + 1) {
                Some('.') => self.operator_token(Operator::RangeInclusive, 2),
                _ => self.operator_token(Operator::Period, 1),
            }),
            ';' => Some(self.separator_token(Separator::Semicolon, 1)),
            '^' => Some(self.operator_token(Operator::Caret, 1)),
            '>' => Some(match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Gte, 2),
                _ => self.operator_token(Operator::Gt, 1),
            }),

            '<' => Some(match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Lte, 2),
                Some('>') => self.operator_token(Operator::NotEquals, 2),
                _ => self.operator_token(Operator::Lt, 1),
            }),

            ':' => Some(match self.line.get(self.location.col + 1) {
                Some('=') => self.operator_token(Operator::Assignment, 2),
                _ => self.separator_token(Separator::Colon, 1),
            }),

            '=' => Some(self.operator_token(Operator::Equals, 1)),
            '$' => Some(self.literal_hex()?),
            '#' => Some(self.literal_int(true)?),
            '0'..='9' => Some(self.literal_int(false)?),
            '\'' => Some(self.literal_string()?),
            'a'..='z' | '_' | 'A'..='Z' => {
                match self.word()? {
                    // keyword "begin" always signals the start of a begin..end delimited group
                    TokenTree::Keyword {
                        kw: Keyword::Begin, ..
                    } => {
                        self.begin_delim_group(DelimiterPair::BeginEnd, 0);
                        None
                    }

                    // keyword "case" always signals the start of a begin..end delimited group
                    TokenTree::Keyword {
                        kw: Keyword::Case, ..
                    } => {
                        self.begin_delim_group(DelimiterPair::CaseEnd, 0);
                        None
                    }

                    TokenTree::Keyword {
                        kw: Keyword::Match, ..
                    } => {
                        self.begin_delim_group(DelimiterPair::MatchEnd, 0);
                        None
                    }

                    TokenTree::Keyword {
                        kw: Keyword::End, ..
                    } if self.in_begin_block() => {
                        Some(self.end_delim_group(DelimiterPair::BeginEnd, 0)?)
                    }

                    TokenTree::Keyword {
                        kw: Keyword::End, ..
                    } if self.in_case_block() => {
                        Some(self.end_delim_group(DelimiterPair::CaseEnd, 0)?)
                    }

                    TokenTree::Keyword {
                        kw: Keyword::End, ..
                    } if self.in_match_block() => {
                        Some(self.end_delim_group(DelimiterPair::MatchEnd, 0)?)
                    }

                    tt => Some(tt),
                }
            }

            bad => {
                let err_start = self.location;
                self.location.col += 1;
                let err_span = self.src_span_from(err_start);
                return Err(TracedError::trace(TokenizeError::IllegalToken(bad.to_string(), err_span)));
            }
        };

        if let Some(tt) = token {
            if self.delim_stack.is_empty() {
                self.line_buf.push(tt);
            } else {
                let last_index = self.delim_stack.len() - 1;
                let current_delim = &mut self.delim_stack[last_index];
                current_delim.inner.push(tt);
            }
        }

        Ok(true)
    }

    fn in_begin_block(&self) -> bool {
        let current_delim = self.delim_stack.last().map(|group| group.delim);
        current_delim == Some(DelimiterPair::BeginEnd)
    }

    fn in_case_block(&self) -> bool {
        let current_delim = self.delim_stack.last().map(|group| group.delim);
        current_delim == Some(DelimiterPair::CaseEnd)
    }

    fn in_match_block(&self) -> bool {
        let current_delim = self.delim_stack.last().map(|group| group.delim);
        current_delim == Some(DelimiterPair::MatchEnd)
    }
}
