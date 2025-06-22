mod alias_decl;
mod const_decl;
mod unit_decl;
mod decl_mod;
mod use_decl;

pub use self::alias_decl::*;
pub use self::const_decl::*;
pub use self::decl_mod::*;
pub use self::unit_decl::*;
pub use self::use_decl::*;
use crate::ast;
use crate::ast::tag::Tag;
use crate::ast::Annotation;
use crate::ast::BindingDeclKind;
use crate::ast::Block;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::IdentPath;
use crate::ast::Stmt;
use crate::ast::TypeDecl;
use crate::ast::TypeDeclItem;
use crate::parse::AggregateParseResult;
use crate::parse::ContinueParse;
pub use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::typ::builtin_span;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::DelimiterPair;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use std::fmt;
use std::sync::Arc;
use terapascal_common::aggregate_err::AggregateError;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum UnitKind {
    Program,
    Library,
    Unit,
}

impl fmt::Display for UnitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitKind::Program => write!(f, "program"),
            UnitKind::Library => write!(f, "library"),
            UnitKind::Unit => write!(f, "unit"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation = Span> {
    pub unit_kw: Option<Span>,
    pub kind: UnitKind,

    pub ident: IdentPath,
    
    pub iface_section: UnitDeclSection<A>,
    pub impl_section: UnitDeclSection<A>,

    pub init: Option<InitBlock<A>>,
    
    pub end_kw: Option<Span>,
}

#[derive(Clone, Debug)]
pub struct InitBlock<A: Annotation = Span> {
    pub keyword_span: Span,
    pub body: Vec<Stmt<A>>,
    
    pub end_span: Option<Span>,
}

/// a block of declarations grouped by visibility, e.g. the "implementation" or "interface" section
#[derive(Clone, Debug)]
pub struct UnitDeclSection<A: Annotation = Span> {
    // may or may not start with a keyword - program/library units have a decl section, but
    // it's always an "implementation" section so no keyword is needed
    pub kw_span: Option<Span>,
    
    pub decls: Vec<UnitDecl<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn all_decls(&self) -> impl Iterator<Item = (Visibility, &UnitDecl<A>)> {
        self.impl_section.decls
            .iter()
            .map(|decl| (Visibility::Interface, decl))
            .chain(
                self.iface_section.decls
                    .iter()
                    .map(|decl| (Visibility::Implementation, decl)),
            )
    }

    pub fn func_decls(&self) -> impl Iterator<Item = (Visibility, &Arc<FunctionDecl<A>>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::FunctionDecl { decl: func, .. } => Some((vis, func)),
            UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, &func_def.decl)),
            _ => None,
        })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = (Visibility, &Arc<FunctionDef<A>>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, func_def)),
            _ => None,
        })
    }

    pub fn type_decls<'a>(&self) -> impl Iterator<Item = (Visibility, &TypeDecl<A>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::Type { decl: ty, .. } => Some((vis, ty)),
            _ => None,
        })
    }

    pub fn type_decl_items(&self) -> impl Iterator<Item = (Visibility, &TypeDeclItem<A>)> {
        self.type_decls()
            .flat_map(|(vis, decl)| decl.items.iter().map(move |item| (vis, item)))
    }
    
    pub fn var_decl_items(&self) -> impl Iterator<Item = (Visibility, &UnitBindingItem<A>)> {
        self.all_decls()
            .filter_map(|(vis, decl)| match decl {
                UnitDecl::Binding { 
                    decl: binding @ UnitBinding { kind: BindingDeclKind::Var, .. }
                } => {
                    Some((vis, binding))
                }
                
                _ => None,
            })
            .flat_map(|(vis, binding)| binding.items
                .iter()
                .map(move |item| (vis, item)))
    }
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream, file_ident: IdentPath) -> AggregateParseResult<Self> {
        let unit_kind_kw_match = Keyword::Unit | Keyword::Program | Keyword::Library;
        
        let mut errors = Vec::new();

        let (unit_kw, unit_kind, ident) = match tokens.match_one_maybe(unit_kind_kw_match.clone()) {
            Some(TokenTree::Keyword { kw, span }) => {
                let ident = IdentPath::parse(tokens)
                    .and_continue_with(&mut errors, || file_ident.clone());

                if file_ident != ident {
                    let err = ParseError::InvalidUnitFilename(ident.path_span());
                    errors.push(TracedError::from(err));
                }
                
                tokens.advance_until(Separator::Semicolon).and_continue(&mut errors);
                tokens.advance(1);

                let kind = match kw {
                    Keyword::Program => UnitKind::Program,
                    Keyword::Library => UnitKind::Library,
                    _ => UnitKind::Unit,
                };

                (Some(span), kind, ident)
            },

            _ => (None, UnitKind::Unit, file_ident),
        };
        
        let mut unit = Unit {
            unit_kw,
            kind: unit_kind,
            ident,
            iface_section: UnitDeclSection {
                kw_span: None,
                decls: Vec::new(),
            },
            impl_section: UnitDeclSection {
                kw_span: None,
                decls: Vec::new(),
            },
            init: None,
            end_kw: None,
        };

        if unit_kind == UnitKind::Program {
            let decls = UnitDecl::parse_seq(tokens, Visibility::Implementation)
                .unwrap_or_else(|err| {
                    let (decls, mut new_errors) = err.unwrap();
                    errors.append(&mut new_errors);
                    decls
                });

            if !decls.is_empty() {
                tokens.advance_until(Separator::Semicolon).and_continue(&mut errors);
            }

            unit.impl_section.decls.extend(decls);

            // instead of a separate init block, program units always have a "main" block
            // after any decls with the usual begin/end keywords
            
            match Block::parse(tokens) {
                Ok(block) => {
                    let end_kw = match tokens.match_one_maybe(Operator::Period) {
                        Some(tt) => block.end.span().to(tt.span()),
                        None => block.end.clone(),
                    };

                    unit.end_kw = Some(end_kw.clone());

                    unit.init = Some(InitBlock {
                        keyword_span: block.begin.clone(),
                        body: vec![Stmt::Block(Box::new(block))],
                        end_span: Some(end_kw),
                    });
                }
                
                Err(err) => {
                    errors.push(err);
                }
            }
        } else {
            let has_interface = parse_decls_section(
                Visibility::Interface,
                &mut unit.iface_section.kw_span,
                &mut unit.iface_section.decls,
                &mut errors,
                tokens
            ).and_continue(&mut errors, false);

            let has_implementation = parse_decls_section(
                Visibility::Implementation,
                &mut unit.impl_section.kw_span,
                &mut unit.impl_section.decls,
                &mut errors,
                tokens,
            ).and_continue(&mut errors, false);

            let init_kw = tokens.match_one_maybe(Keyword::Initialization);
            if let Some(init_kw) = &init_kw {
                let init_body = parse_init_section(tokens)
                    .and_continue_with(&mut errors, Vec::new);

                let end_kw = match_unit_end(tokens)
                    .map(Some)
                    .and_continue(&mut errors, None);

                unit.end_kw = end_kw.clone();
                
                unit.init = Some(InitBlock {
                    keyword_span: init_kw.span().clone(),
                    body: init_body,
                    end_span: end_kw,
                });
            } else {
                unit.end_kw = match_unit_end(tokens).map(Some).and_continue(&mut errors, None);
            }

            if !(has_interface || has_implementation || init_kw.is_some()) {
                // empty units are invalid! use this to throw an error
                let Err(err) = tokens.match_one(unit_kind_kw_match
                    | Keyword::Interface
                    | Keyword::Implementation
                    | Keyword::Initialization) else {
                    unreachable!();
                };
    
                errors.push(err);
            }
        }
        
        // add auto refs
        add_auto_ref_paths(&unit.ident, &mut unit.iface_section.decls);
        
        AggregateError::result(unit, errors)
    }
}

fn match_unit_end(tokens: &mut TokenStream) -> ParseResult<Span> {
    let span = tokens
        .match_one(Keyword::End)?
        .into_span();

    // allow the traditional period after the final end, but don't require it
    if let Some(period) = tokens.match_one_maybe(Operator::Period) {
        Ok(span.to(period.span()))
    } else {
        Ok(span)
    }
}

fn parse_decls_section(
    visibility: Visibility,
    out_kw: &mut Option<Span>,
    out_decls: &mut Vec<UnitDecl<Span>>,
    errors: &mut Vec<TracedError<ParseError>>,
    tokens: &mut TokenStream,
) -> ParseResult<bool> {
    let Some(kw_tt) = tokens.match_one_maybe(visibility.keyword()) else {
        return Ok(false)
    };

    *out_kw = Some(kw_tt.into_span());

    let decls = UnitDecl::parse_seq(tokens, visibility)
        .unwrap_or_else(|err| {
            let (decls, mut new_errors) = err.unwrap();
            errors.append(&mut new_errors);
            decls
        });

    if !decls.is_empty() {
        tokens.match_one(Separator::Semicolon)?;
    }

    out_decls.extend(decls);

    Ok(true)
}

fn unit_binding_start_matcher() -> Matcher {
    Keyword::Const | Keyword::Var
}

pub(crate) fn func_decl_kw_matcher() -> Matcher {
    Keyword::Function
        | Keyword::Class
        | Keyword::Procedure
        | Keyword::Constructor
        | Keyword::Destructor
}

pub(crate) fn unit_func_decl_start_matcher() -> Matcher {
    func_decl_kw_matcher()
        | DelimiterPair::SquareBracket
}

fn parse_unit_decl(tokens: &mut TokenStream, visibility: Visibility) -> ParseResult<UnitDecl> {
    let decl_start = UnitDecl::start_matcher();

    let decl = match tokens.look_ahead().match_one(decl_start) {
        Some(tt) if unit_func_decl_start_matcher().is_match(&tt) => {
            parse_unit_func_decl(tokens, visibility)?
        },

        Some(tt) if tt.is_keyword(Keyword::Type) => UnitDecl::Type {
            decl: TypeDecl::parse(tokens)?,
        },

        Some(tt) if tt.is_keyword(Keyword::Uses) => UnitDecl::Uses {
            decl: UseDecl::parse(tokens)?,
        },

        Some(tt) if unit_binding_start_matcher().is_match(&tt) => UnitDecl::Binding {
            decl: UnitBinding::parse(tokens)?,
        },

        Some(unexpected_tt) => {
            let err = ParseError::UnexpectedToken(
                Box::new(unexpected_tt.clone()),
                Some(UnitDecl::start_matcher()),
            );
            return Err(TracedError::trace(err));
        },

        None => {
            let expected = UnitDecl::start_matcher();
            let err = ParseError::UnexpectedEOF(expected, tokens.context().clone());
            return Err(TracedError::trace(err));
        },
    };

    Ok(decl)
}

fn parse_unit_func_decl(tokens: &mut TokenStream, visibility: Visibility) -> ParseResult<UnitDecl> {
    let tags = Tag::parse_seq(tokens)?;
    let func_decl = Arc::new(FunctionDecl::parse(tokens, true, tags)?);

    let body_ahead = if visibility == Visibility::Interface {
        // interface funcs - never expect a body, unless the function is marked `inline`
        func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::Inline(..) => true,
            _ => false,
        })
    } else {
        // implementation funcs - always expect a body, unless the function has the
        // `external` (body is external) or `forward` (body to follow later) modifiers
        !func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::External { .. } | DeclMod::Forward(..) => true,
            _ => false,
        })
    };

    if body_ahead {
        tokens.match_one(Separator::Semicolon)?;

        let def = FunctionDef::parse_body_of_decl(func_decl, tokens)?;

        Ok(UnitDecl::FunctionDef { def: Arc::new(def) })
    } else {
        Ok(UnitDecl::FunctionDecl { decl: func_decl })
    }
}

fn parse_init_section(tokens: &mut TokenStream) -> ParseResult<Vec<Stmt<Span>>> {
    let stmts = Stmt::parse_seq(tokens)?;

    // the last stmt may be optionally terminated with a redundant separator
    if stmts.len() > 0 {
        tokens.match_one_maybe(Separator::Semicolon);
    }

    Ok(stmts)
}

impl<A: Annotation> fmt::Display for Unit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            UnitKind::Program => write!(f, "program ")?,
            UnitKind::Library => write!(f, "library ")?,
            UnitKind::Unit => write!(f, "unit ")?,
        }

        writeln!(f, "{};", self.ident)?;

        if !self.iface_section.decls.is_empty() {
            writeln!(f, "interface")?;
            writeln!(f)?;

            for decl in &self.iface_section.decls {
                writeln!(f, "{};", decl)?;
            }
            writeln!(f)?;
        }

        if self.kind == UnitKind::Unit {
            writeln!(f, "implementation")?;
            writeln!(f)?;
        }

        for decl in &self.impl_section.decls {
            writeln!(f, "{};", decl)?;
        }
        writeln!(f)?;
        
        if let Some(init_block) = &self.init {
            if self.kind == UnitKind::Unit {
                writeln!(f, "initialization")?;
            } else {
                writeln!(f, "begin")?;
            }
            
            for init_stmt in &init_block.body {
                writeln!(f, "\t{};", init_stmt)?;
            }
        }

        writeln!(f, "end.")?;

        Ok(())
    }
}

// auto-ref namespaces are implicitly used by every compiled unit (e.g. "System").
// if a unit's interface section doesn't literally contain a using decl for these units, 
// we synthesize new using decls and add them.
// the parser does this (rather than adding implicit used units in the typechecking scope) because:
// a) we look at parsed units to determine compilation order (until project is implemented)
// b) they appear in the printed AST when outputting it this way, which is nice for debugging
static AUTO_REF_NAMESPACES: [&str; 1] = [
    SYSTEM_UNIT_NAME
];

fn auto_ref_namespaces() -> Vec<IdentPath> {
    AUTO_REF_NAMESPACES
        .iter()
        .map(|auto_ref_name| {
            IdentPath::from_parts(auto_ref_name
                .split('.')
                .map(|part| ast::Ident::new(part, builtin_span())))
        })
        .collect()
}

fn add_auto_ref_paths(unit_namespace: &IdentPath, unit_decls: &mut Vec<UnitDecl<Span>>) {
    let auto_ref_paths = auto_ref_namespaces();
    for (i, auto_ref_path) in auto_ref_paths.into_iter().enumerate() {
        if auto_ref_path == *unit_namespace {
            continue;
        }
        
        let has_using = unit_decls
            .iter()
            .flat_map(|decl| match decl {
                UnitDecl::Uses { decl } => decl.units.as_slice(),
                _ => &[],
            })
            .any(|use_decl| use_decl.ident == auto_ref_path);

        if !has_using {
            unit_decls.insert(i, UnitDecl::Uses {
                decl: UseDecl {
                    units: vec![UseDeclItem {
                        ident: auto_ref_path,
                        span: builtin_span(),
                        path: None,
                    }],
                    span: builtin_span(),
                }
            })
        }
    }
}
