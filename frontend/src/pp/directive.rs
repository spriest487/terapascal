use regex::Regex;
use common::LanguageMode;

pub enum Directive {
    Define(String),
    Undef(String),
    IfDef(String),
    IfOpt(String, bool),
    IfNDef(String),
    EndIf {
        #[allow(unused)]
        comment: String 
    },
    Else,
    ElseIf(String),
    Mode(LanguageMode),
    Switch(String, bool),
    LinkLib(String),
    Include(String),
}

pub struct DirectiveParser {
    define_pattern: Regex,
    undef_pattern: Regex,
    ifdef_pattern: Regex,
    ifopt_pattern: Regex,
    ifndef_pattern: Regex,
    endif_pattern: Regex,
    else_pattern: Regex,
    elseif_pattern: Regex,
    mode_pattern: Regex,
    switch_pattern: Regex,
    linklib_pattern: Regex,
    include_pattern: Regex,
}

impl DirectiveParser {
    pub fn new() -> Self {
        Self {
            define_pattern: Regex::new(r"(?i)^define\s+(\w+)$").unwrap(),
            undef_pattern: Regex::new(r"(?i)^undef\s+(\w+)$").unwrap(),
            ifdef_pattern: Regex::new(r"(?i)^ifdef\s+(\w+)$").unwrap(),
            ifopt_pattern: Regex::new(r"(?i)^ifopt\s+(\w+)([+-])$").unwrap(),
            ifndef_pattern: Regex::new(r"(?i)^ifndef\s+(\w+)$").unwrap(),
            endif_pattern: Regex::new(r"(?i)^endif(\s.+)?$").unwrap(),
            else_pattern: Regex::new("(?i)^else$").unwrap(),
            elseif_pattern: Regex::new(r"(?i)^elseif\s+(\w+)$").unwrap(),
            mode_pattern: Regex::new(r"(?i)^mode\s+(\w+)$").unwrap(),
            switch_pattern: Regex::new(r"(?i)^([a-zA-Z]+)([+-])$").unwrap(),
            linklib_pattern: Regex::new(r"(?i)^linklib\s+(.+)$").unwrap(),
            include_pattern: Regex::new(r"(?i)^i(nclude)?\s+(.+)$").unwrap(),
        }
    }

    pub fn parse(&self, s: &str) -> Option<Directive> {
        if let Some(define_captures) = self.define_pattern.captures(s) {
            let symbol = define_captures[1].to_string();

            Some(Directive::Define(symbol))
        } else if let Some(undef_captures) = self.undef_pattern.captures(s) {
            let symbol = undef_captures[1].to_string();

            Some(Directive::Undef(symbol))
        } else if let Some(ifdef_captures) = self.ifdef_pattern.captures(s) {
            let symbol = ifdef_captures[1].to_string();

            Some(Directive::IfDef(symbol))
        } else if let Some(ifopt_captures) = self.ifopt_pattern.captures(s) {
            let switch = ifopt_captures[1].to_string();
            let on = match &ifopt_captures[2] {
                "-" => false,
                _ => true,
            };

            Some(Directive::IfOpt(switch, on))
        } else if let Some(ifndef_captures) = self.ifndef_pattern.captures(s) {
            let symbol = ifndef_captures[1].to_string();

            Some(Directive::IfNDef(symbol))
        } else if let Some(endif_captures) = self.endif_pattern.captures(s) {
            Some(Directive::EndIf { 
                comment: endif_captures.get(1)
                    .map(|m| m.as_str().trim().to_string())
                    .unwrap_or_else(String::new)
            })
        } else if self.else_pattern.is_match(s) {
            Some(Directive::Else)
        } else if let Some(elseif_captures) = self.elseif_pattern.captures(s) {
            let symbol = elseif_captures[1].to_string();

            Some(Directive::ElseIf(symbol))
        } else if let Some(mode_captures) = self.mode_pattern.captures(s) {
            let mode = match mode_captures[1].to_uppercase().as_str() {
                "FPC" => LanguageMode::Fpc,
                "TERA" => LanguageMode::Default,
                "DELPHI" => LanguageMode::Delphi,
                unrecognized => {
                    eprintln!("unrecognized mode: {}", unrecognized);
                    return None;
                }
            };

            Some(Directive::Mode(mode))
        } else if let Some(switch_capture) = self.switch_pattern.captures(s) {
            let switch_name = switch_capture[1].to_string();
            let switch_val = match &switch_capture[2] {
                "+" => true,
                "-" => false,
                _ => unreachable!("excluded by pattern: {}", &switch_capture[2]),
            };

            Some(Directive::Switch(switch_name, switch_val))
        } else if let Some(linklib) = self.linklib_pattern.captures(s) {
            Some(Directive::LinkLib(linklib[1].to_string()))
        } else if let Some(include) = self.include_pattern.captures(s) {
            Some(Directive::Include(include[2].to_string()))
        } else {
            None
        }
    }
}
