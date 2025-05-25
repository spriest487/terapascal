use crate::ast;
use crate::ast::util::tokens_from_string;
use crate::ast::IdentPath;
use crate::ast::TypeName;
use crate::parse::Parse;
use crate::pp::Preprocessor;
use crate::typ::ast::OverloadCandidate;
use crate::typ::test::module_from_src;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::Module;
use crate::typ::Symbol;
use crate::typ::TypeArgList;
use crate::TokenStream;
use crate::TokenTree;
use terapascal_common::span::Span;
use terapascal_common::BuildOptions;

pub fn expr_from_str(src: &str) -> ast::Expr<Span> {
    let test_unit = Preprocessor::new("test", BuildOptions::default()).preprocess(src).unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();

    let mut tokens = TokenStream::new(tokens, Span::zero("test"));

    ast::Expr::parse(&mut tokens)
        .and_then(|expr| {
            tokens.finish()?;
            Ok(expr)
        })
        .unwrap()
}

pub fn type_args_from_str(args: &[&str], ctx: &mut Context) -> TypeArgList {
    let arg_types: Vec<_> = args
        .iter()
        .map(|arg_str| {
            let mut tokens = tokens_from_string("type_args_from_str", arg_str);
            let type_name = TypeName::parse(&mut tokens).unwrap();

            typecheck_type(&type_name, ctx).unwrap()
        })
        .collect();

    TypeArgList::new(arg_types, Span::zero("type_args_from_str"))
}

pub fn candidates_from_module(module: &Module, unit_name: &str) -> Vec<OverloadCandidate> {
    let unit =
        module.units.iter().find(|unit| unit.unit.ident.last().name.as_str() == unit_name).unwrap();

    let candidates = unit.unit.func_defs().map(|(visibility, func)| {
        let sig = func.decl.sig();

        match &func.decl.name.owning_ty {
            Some(explicit_impl) => {
                let ident = func.decl.name.ident.clone();
                let (method_index, method) = explicit_impl
                    .find_method(&ident, &sig, &unit.context)
                    .unwrap()
                    .expect("method defs in unit must have a corresponding decl in the type");

                OverloadCandidate::Method {
                    index: method_index,
                    self_ty: explicit_impl.clone(),
                    iface_ty: explicit_impl.clone(),
                    decl: method,
                }
            },

            None => {
                let decl_name = IdentPath::from(func.decl.name.ident.clone());

                OverloadCandidate::Function {
                    decl_name: Symbol::from(decl_name),
                    visibility,
                    decl: func.decl.clone(),
                }
            },
        }
    });

    candidates.collect()
}

pub fn candidates_from_src(
    src: &'static str,
    unit_name: &str,
) -> (Vec<OverloadCandidate>, Context) {
    let module = module_from_src(unit_name, src);

    let candidates = candidates_from_module(&module, unit_name);

    (
        candidates,
        module
            .units
            .into_iter()
            .filter_map(|unit| {
                if unit.unit.ident.last().name.as_str() == unit_name {
                    Some(unit.context)
                } else {
                    None
                }
            })
            .next()
            .unwrap(),
    )
}
