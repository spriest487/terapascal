use crate::ast;
use crate::typ::ast::const_eval_string;
use crate::typ::ast::typecheck_expr;
use crate::typ::string_type;
use crate::typ::Context;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type DeclMod = ast::DeclMod<Value>;

impl DeclMod {
    pub fn typecheck_mods(decl: &ast::FunctionDecl, is_method: bool, ctx: &mut Context) -> TypeResult<Vec<Self>> {
        let mut results: Vec<DeclMod> = Vec::new();
        
        // incompatible modifiers
        let mut external = None;
        let mut overload = None;
        
        let mut invalid_method_mods = Vec::new();

        for decl_mod in &decl.mods {
            // check uniqueness
            let existing = results
                .iter()
                .find(|existing_mod| existing_mod.keyword() == decl_mod.keyword());
            
            if let Some(existing_mod) = existing {
                return Err(TypeError::DuplicateDeclMod {
                    keyword: decl_mod.keyword().to_string(),
                    span: decl_mod.span().clone(),
                    existing: existing_mod.span().clone(),
                });
            }
            
            let result = match decl_mod {
                ast::DeclMod::External { src, span, kw_span } => {
                    check_incompatible_mod(&overload, Self::OVERLOAD_WORD, span, Self::EXTERNAL_WORD)?;

                    if let Some(decl_type_params) = &decl.name.type_params {
                        let ty_args_span = decl_type_params.items[0].span()
                            .to(decl_type_params.items
                                .last()
                                .unwrap()
                                .span());
                        return Err(TypeError::ExternalGenericFunction {
                            func: decl.name.ident.clone(),
                            extern_modifier: span.clone(),
                            ty_args: ty_args_span,
                        });
                    }

                    external = Some(span.clone());
                    
                    let string_ty = string_type(ctx)?;

                    let src = typecheck_expr(src, &string_ty, ctx)?;
                    let src_str = const_eval_string(&src, ctx)?;

                    let extern_mod = DeclMod::External {
                        src: src_str,
                        kw_span: kw_span.clone(),
                        span: span.clone(),
                    };

                    if is_method {
                        invalid_method_mods.push(extern_mod.clone());
                    }

                    extern_mod
                }

                ast::DeclMod::Inline(span) => {
                    let inline_mod = DeclMod::Inline(span.clone());
                    if is_method {
                        invalid_method_mods.push(inline_mod.clone());
                    }

                    inline_mod
                },

                ast::DeclMod::Forward(span) => {
                    let forward_mod = DeclMod::Forward(span.clone());
                    if is_method {
                        invalid_method_mods.push(forward_mod.clone());
                    }

                    forward_mod
                },

                ast::DeclMod::Overload(span) => {
                    check_incompatible_mod(&external, Self::EXTERNAL_WORD, span, Self::OVERLOAD_WORD)?;
                    
                    overload = Some(span.clone());
                    DeclMod::Overload(span.clone())
                },
            };

            results.push(result);
        }

        if is_method && !invalid_method_mods.is_empty() {
            return Err(TypeError::InvalidMethodModifiers {
                mods: invalid_method_mods,
                span: decl.span.clone(),
            })
        }

        Ok(results)
    }
}

fn check_incompatible_mod(first_span: &Option<Span>, first_kw: &str, span: &Span, kw: &str) -> TypeResult<()> {
    if let Some(first_span) = first_span {
        return Err(TypeError::IncompatibleDeclMod {
            first_keyword: first_kw.to_string(),
            first_span: first_span.clone(),
            second_keyword: kw.to_string(),
            second_span: span.clone(),
        });
    }
    
    Ok(())
}
