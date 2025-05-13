use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Unit;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::pp::Preprocessor;
use crate::TokenTree;
use common::span::Span;
use common::BuildOptions;
use common::DiagnosticLabel;
use common::DiagnosticOutput;

pub fn try_tokens_from_string(unit_name: &str, src: &str) -> Result<TokenStream, String> {
    let pp = Preprocessor::new(format!("{}.pas", unit_name), BuildOptions::default());
    let pp_unit = pp.preprocess(src)
        .map_err(|err| err.to_string())?;

    let tokens = TokenTree::tokenize(pp_unit)
        .map_err(|err| err.to_string())?;

    let context = tokens
        .get(0)
        .map(|tt| tt.span().clone())
        .unwrap_or_else(|| Span::zero(unit_name));
    
    Ok(TokenStream::new(tokens, context))
}

pub fn tokens_from_string(unit_name: &str, src: &str) -> TokenStream {
    try_tokens_from_string(unit_name, src).unwrap()
}

pub fn try_parse_from_string<T: Parse>(unit_name: &str, src: &str) -> ParseResult<T> {
    let mut tokens = tokens_from_string(unit_name, src);
    let result = T::parse(&mut tokens)?;
    tokens.finish()?;
    
    Ok(result)
}

pub fn try_unit_from_string(unit_name: &str, src: &str) -> ParseResult<Unit<Span>> {
    let mut tokens = tokens_from_string(unit_name, src);

    let unit_ident = Ident::new(unit_name, Span::zero(unit_name));

    let unit = Unit::parse(&mut tokens, IdentPath::from_parts(vec![unit_ident]))?;
    tokens.finish()?;

    Ok(unit)
}

pub fn unit_from_string(unit_name: &str, src: &str) -> Unit<Span> {
    try_unit_from_string(unit_name, src)
        .unwrap_or_else(|traced| {
            match &traced.err.label() {
                Some(DiagnosticLabel { text: Some(text), span }) => {
                    panic!("{} ({})\n{}", traced.err, span, text)
                },
                Some(DiagnosticLabel { text: None, span }) => {
                    panic!("{} ({})", traced.err, span)
                },
                None => panic!("{}", traced.err)
            }
        })
}
