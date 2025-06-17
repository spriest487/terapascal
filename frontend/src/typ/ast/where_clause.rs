use crate::ast;
use crate::ast::TypeAnnotation;
use crate::typ::typecheck_typename;
use crate::typ::Context;
use crate::typ::TypeName;
use crate::typ::GenericContext;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeParam;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use crate::typ::GenericError;
use crate::typ::Value;
use crate::Ident;
use std::mem;

pub type WhereClause = ast::WhereClause<Value>;
pub type TypeConstraint = ast::TypeConstraint<Value>;

impl WhereClause {
    pub fn typecheck(src: &ast::WhereClause, ctx: &mut Context) -> TypeResult<Self> {
        let mut constraints = Vec::new();
        for constraint in &src.constraints {
            let is_ty = if constraint.is_ty.is_known() {
                typecheck_typename(&constraint.is_ty, ctx)?
            } else {
                TypeName::inferred(Type::Any)
            };

            constraints.push(TypeConstraint {
                is_kw_span: constraint.is_kw_span.clone(),
                
                is_ty,

                name: constraint.name.clone(),
                span: constraint.span.clone(),
            })
        }

        Ok(Self {
            where_kw_span: src.where_kw_span.clone(),
            constraints,
            span: src.span.clone(),
        })
    }

    pub fn typecheck_constrained_params(
        self,
        param_names: ast::TypeList<Ident>,
    ) -> TypeResult<TypeParamList> {
        let param_count = param_names.len();
        let span = param_names.span;

        // match parsed constraints in the where clause to type params in the param list by
        // ident. if a param has no constraint with the same ident, it gets a None constraint instead
        let mut constrained_params: Vec<TypeParam> = Vec::with_capacity(param_names.items.len());
        
        let mut unmatched_constraints = self.constraints;

        for type_param_ident in param_names.items {
            let constraint_index = unmatched_constraints
                .iter()
                .position(|c| c.name == type_param_ident);
            
            let constraint = match constraint_index {
                Some(i) => {
                    let matched_constraint = unmatched_constraints.remove(i);
                    
                    let duplicate = constrained_params
                        .iter()
                        .any(|p| p.name == type_param_ident);
                    
                    if duplicate {
                        return Err(TypeError::from_generic_err(GenericError::DuplicateConstraint { 
                            constraint: matched_constraint.name 
                        }, matched_constraint.span));
                    }

                    Some(matched_constraint)
                },
                None => None,
            };

            constrained_params.push(TypeParam {
                name: type_param_ident.clone(),
                constraint,
                span: type_param_ident.span.clone(),
            });
        }

        if !unmatched_constraints.is_empty() {
            // just error on the first one
            let first_bad = unmatched_constraints.remove(0);
            
            let is_duplicate = constrained_params
                .iter()
                .any(|p| p.name == first_bad.name);
            
            let err = if is_duplicate {
                GenericError::DuplicateConstraint { constraint: first_bad.name }
            } else {
                GenericError::UnexpectedConstraint { constraint: first_bad.name }
            };

            return Err(TypeError::from_generic_err(err, first_bad.span));
        }

        assert_eq!(constrained_params.len(), param_count);

        Ok(ast::TypeList::new(constrained_params, span))
    }

    pub fn apply_generics(mut self, generic_ctx: &GenericContext) -> Self {
        for constraint in &mut self.constraints {
            let mut is_ty = TypeName::inferred(Type::Nothing);
            mem::swap(&mut is_ty, &mut constraint.is_ty);

            constraint.is_ty = generic_ctx.apply_to_typename(is_ty);
        }

        self
    }
}
