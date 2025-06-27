extern crate core;

pub mod ast;
pub mod codegen;
pub mod consts;
pub mod parse;
pub mod pp;
pub mod result;
pub mod token_tree;
pub mod typ;

pub use self::consts::EnumConstant;
pub use self::consts::IntConstant;
pub use self::consts::RealConstant;
pub use self::consts::SetConstant;
pub use self::token_tree::DelimiterPair;
pub use self::token_tree::Separator;
pub use self::token_tree::TokenStream;
pub use self::token_tree::TokenTree;
pub use self::token_tree::TokenizeError;
pub use self::token_tree::TokenizeResult;
pub use ast::keyword::Keyword;
pub use ast::operators::CompoundAssignmentOperator;
pub use ast::operators::Operator;
pub use ast::operators::Position;

use crate::codegen::CodegenOpts;
use crate::parse::AggregateParseError;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::Parser;
use crate::pp::error::PreprocessorError;
use crate::pp::PreprocessedUnit;
use crate::typ::Module;
use crate::typ::TypeResult;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::aggregate_err::AggregateError;
use terapascal_common::build_log::BuildLog;
use terapascal_common::fs::Filesystem;
use terapascal_common::span::Location;
use terapascal_common::span::Span;
use terapascal_common::CompileOpts;
use terapascal_common::TracedError;
use terapascal_ir as ir;

pub fn preprocess(
    fs: &impl Filesystem,
    filename: impl Into<PathBuf>,
    src: &str,
    opts: CompileOpts,
) -> Result<PreprocessedUnit, PreprocessorError> {
    let pp = pp::Preprocessor::new(fs, filename, opts);
    pp.preprocess(&src)
}

pub fn tokenize(unit: PreprocessedUnit) -> TokenizeResult<Vec<TokenTree>> {
    TokenTree::tokenize(unit)
}

pub fn parse(
    filename: impl Into<PathBuf>,
    tokens: impl IntoIterator<Item = TokenTree>,
) -> ParseResult<ast::Unit> {
    let file_span = Span {
        file: Arc::new(filename.into()),
        start: Location::zero(),
        end: Location::zero(),
    };

    let unit_ident = file_span
        .file
        .with_extension("")
        .file_name()
        .map(|file_name| {
            let unit_ident = ast::IdentPath::from_parts(
                file_name
                    .to_string_lossy()
                    .split('.')
                    .map(|part| ast::Ident::new(part, file_span.clone())),
            );

            unit_ident
        })
        .ok_or_else(|| {
            let err = ParseError::InvalidUnitFilename(file_span.clone());
            TracedError::trace(err)
        })?;

    let tokens = TokenStream::new(tokens, file_span);
    let mut parser = Parser::new(tokens);

    let unit = ast::Unit::parse(&mut parser, unit_ident);

    let errors = parser.finish();

    AggregateParseError::result(unit, errors).map_err(AggregateError::into_err)
}

pub fn typecheck<'a>(
    units: impl DoubleEndedIterator<Item = (&'a PathBuf, &'a ast::Unit)>,
    verbose: bool,
    log: &mut BuildLog,
) -> TypeResult<Module> {
    let module = Module::typecheck(units, verbose, log)?;

    for error in module.root_ctx.errors() {
        log.diagnostic(error.clone());
    }

    Ok(module)
}

pub fn codegen_ir(module: &Module, opts: CodegenOpts) -> ir::Library {
    codegen::gen_lib(module, opts)
}
