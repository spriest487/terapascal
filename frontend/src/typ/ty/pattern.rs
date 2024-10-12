use std::fmt;
use std::rc::Rc;
use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::typ::context;
use crate::typ::result::*;
use crate::typ::ty::typecheck_type;
use crate::typ::ty::Specializable;
use crate::typ::ty::Type;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::NameResult;
use crate::typ::Symbol;
use common::span::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypePattern {
    VariantCase {
        variant: Rc<Symbol>,
        case: Ident,
        data_binding: Option<Ident>,
        span: Span,
    },
    NegatedVariantCase {
        variant: Rc<Symbol>,
        case: Ident,
        span: Span,
    },
    Type {
        ty: Type,
        binding: Option<Ident>,
        span: Span,
    },
    NegatedType {
        ty: Type,
        span: Span,
    },
}

impl Spanned for TypePattern {
    fn span(&self) -> &Span {
        match self {
            TypePattern::VariantCase { span, .. } => span,
            TypePattern::NegatedVariantCase { span, .. } => span,
            TypePattern::Type { span, .. } => span,
            TypePattern::NegatedType { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PatternBinding {
    pub ident: Ident,
    pub ty: Type,
}

impl TypePattern {
    // for an identPath (eg System.Option.Some) check if the last part is a declared variant
    // case and return the separate variant ident (eg System.Option) and case ident (eg Some)
    fn find_variant_case(
        path: &IdentPath,
        ctx: &Context,
    ) -> TypeResult<Option<(IdentPath, Ident)>> {
        let variant_parts = path.iter().cloned().take(path.as_slice().len() - 1);
        let stem_name = IdentPath::from_parts(variant_parts);
        let case_ident = path.last();

        match ctx.find_path(&stem_name) {
            Some(context::ScopeMemberRef::Decl {
                     value: Decl::Type { ty, .. },
                     ..
                 }) if ty.as_variant().is_ok() => {
                let variant = ty.as_variant().unwrap();
                let variant_def = ctx.find_variant_def(&variant.full_path).unwrap();

                // check case with this ident exists
                if variant_def.case_position(case_ident).is_none() {
                    let err_span = path.first().span.to(&case_ident.span);
                    let variant_ty = Type::variant(variant_def.name.clone());

                    return Err(TypeError::from_name_err(NameError::MemberNotFound {
                        base: NameContainer::Type(variant_ty),
                        member: case_ident.clone(),
                    }, err_span));
                }

                Ok(Some((
                    variant_def.name.full_path.clone(),
                    case_ident.clone(),
                )))
            }

            _ => Ok(None),
        }
    }

    // patterns referring to a typename can use a generic typename as the pattern, in which
    // case we infer the specialized type (or throw NotMatchable for generics we can't infer a
    // proper specialization for)
    fn find_pattern_ty(
        name: &ast::TypeName,
        span: &Span,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<Type> {
        let raw_ty = match name {
            ast::TypeName::Ident(IdentTypeName { ident, .. }) => {
                let (_ident_path, ty) = ctx.find_type(ident)
                    .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

                ty.clone()
            },
            _ => typecheck_type(name, ctx)?,
        };

        let matchable_ty = if raw_ty.is_matchable() {
            raw_ty.infer_specialized_from_hint(expect_ty).cloned()
        } else {
            None
        };

        matchable_ty.ok_or_else(|| TypeError::NotMatchable {
            ty: raw_ty.clone(),
            span: span.clone(),
        })
    }

    // find_pattern_ty, but for variant type patterns
    fn find_pattern_variant(
        variant: &IdentPath,
        span: &Span,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<Rc<Symbol>> {
        match expect_ty {
            expect_var @ Type::Variant(..) => {
                let variant_def = ctx.find_variant_def(variant)
                    .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

                let var_ty = Type::variant(variant_def.name.clone());

                var_ty
                    .infer_specialized_from_hint(expect_var)
                    .map(|ty| match ty {
                        Type::Variant(v) => {
                            v.clone()
                        },
                        _ => unreachable!("should never infer a non-variant specialized type for a generic variant"),
                    })
                    .ok_or_else(|| TypeError::UnableToInferSpecialization {
                        generic_ty: var_ty.clone(),
                        hint_ty: expect_ty.clone(),
                        span: span.clone(),
                    })
            }

            _ => {
                let variant_def = ctx.find_variant_def(variant)
                    .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

                // expect_ty is probably Nothing and we have to assume the type we find from
                // just the typename is right (if not, we'll get a type mismatch later)
                // todo: make sure we get a type mismatch later
                Ok(variant_def.name.clone())
            }
        }
    }

    pub fn typecheck(
        pattern: &ast::TypeNamePattern,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<TypePattern> {
        match pattern {
            // this pattern typename will never contain generic args (we can't parse those here),
            // so either this is a non-generic type or we'll infer a specialization from the
            // expr's expected type
            ast::TypeNamePattern::TypeName {
                name,
                kind,
                span,
            } => {
                let span = span.clone();

                match Self::find_variant_case(name, ctx)? {
                    Some((variant, case)) => {
                        let variant = Self::find_pattern_variant(&variant, &span, expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let data_binding = kind.binding().cloned();
                                Ok(TypePattern::VariantCase { variant, case, data_binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedVariantCase { variant, case, span, })
                            }
                        }
                    }

                    None => {
                        let ty_name = ast::TypeName::Ident(IdentTypeName {
                            span: name.span().clone(),
                            indirection: 0,
                            type_args: None,
                            ident: name.clone(),
                        });

                        let ty = Self::find_pattern_ty(&ty_name, pattern.span(), expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let binding = kind.binding().cloned();
                                Ok(TypePattern::Type { ty, binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedType { ty, span, })
                            }
                        }
                    }
                }
            },

            ast::TypeNamePattern::ExactType { name, kind, span } => {
                let span = span.clone();

                let ty = Self::find_pattern_ty(name, &span, expect_ty, ctx)?;

                match kind {
                    ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                        let binding = kind.binding().cloned();
                        Ok(TypePattern::Type { ty, binding, span, })
                    }
                    ast::TypeNamePatternKind::IsNot => {
                        Ok(TypePattern::NegatedType { ty, span, })
                    }
                }
            }
        }
    }

    pub fn bindings(&self, ctx: &mut Context) -> NameResult<Vec<PatternBinding>> {
        match self {
            TypePattern::Type {
                ty,
                binding: Some(ident),
                ..
            } => {
                let binding = PatternBinding {
                    ident: ident.clone(),
                    ty: ty.clone(),
                };
                Ok(vec![binding])
            }

            TypePattern::VariantCase {
                variant,
                case,
                data_binding: Some(ident),
                ..
            } => {
                let variant_def = ctx.instantiate_variant_def(variant)?;
                let case_ty = variant_def.cases.iter()
                    .find_map(|c| if c.ident == *case {
                        c.data_ty.clone()
                    } else {
                        None
                    })
                    .expect("variant case pattern with a binding must always reference a case which has a data member");

                let binding = PatternBinding {
                    ty: case_ty,
                    ident: ident.clone(),
                };
                Ok(vec![binding])
            }

            _ => Ok(Vec::new()),
        }
    }
}

impl fmt::Display for TypePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypePattern::Type { ty, binding, .. } => {
                write!(f, "{}", ty)?;
                if let Some(binding) = binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedType { ty, .. } => write!(f, "not {}", ty),

            TypePattern::VariantCase {
                variant,
                case,
                data_binding,
                ..
            } => {
                write!(f, "{}.{}", variant.full_path, case)?;
                if let Some(binding) = data_binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedVariantCase { variant, case, .. } => {
                write!(f, "not {}.{}", variant.full_path, case)
            }
        }
    }
}
