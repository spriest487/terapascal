use crate::ast;
use crate::ast::FunctionParamMod;
use crate::typ::ast::AnonymousFunctionDef;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionCall;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionParam;
use crate::typ::ast::MethodCallNoArgs;
use crate::typ::GenericResult;
use crate::typ::GenericError;
use crate::typ::Context;
use crate::typ::GenericTarget;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParam;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::Value;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSigParam {
    pub modifier: Option<FunctionParamMod>,
    pub ty: Type,
}

impl FunctionSigParam {
    pub fn by_val(ty: Type) -> Self {
        Self { ty, modifier: None }
    }

    pub fn inout(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Var),
        }
    }

    pub fn out(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Out),
        }
    }

    pub fn is_by_ref(&self) -> bool {
        match &self.modifier {
            Some(FunctionParamMod::Out) | Some(FunctionParamMod::Var) => true,
            _ => false,
        }
    }
    
    pub fn from_decl_param(param: FunctionParam) -> Self {
        Self {
            ty: param.ty,
            modifier: param.modifier,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSigTypeParam {
    pub is_ty: Type,
}

impl FunctionSigTypeParam {
    pub fn from_type_param(param: TypeParam) -> Self {
        FunctionSigTypeParam {
            is_ty: param.constraint
                .as_ref()
                .map(|constraint| &constraint.is_ty)
                .cloned()
                .unwrap_or(Type::Any)
        }
    }
}

pub type FunctionSigTypeParamList = ast::TypeList<FunctionSigTypeParam>;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub params: Vec<FunctionSigParam>,
    pub type_params: Option<ast::TypeList<FunctionSigTypeParam>>,
}

impl FunctionSig {
    pub fn new(
        return_ty: Type,
        params: Vec<FunctionSigParam>,
        type_params: Option<TypeParamList>,
    ) -> Self {
        let params = params
            .into_iter()
            .map(|p| FunctionSigParam {
                ty: p.ty.clone(),
                modifier: p.modifier.clone(),
            })
            .collect();

        let type_params = match type_params {
            Some(type_params) => {
                let items: Vec<_> = type_params
                    .items
                    .iter()
                    .map(|decl_param| {
                        let is_ty = decl_param
                            .constraint
                            .as_ref()
                            .map(|c| c.is_ty.clone())
                            .unwrap_or(Type::Any);

                        FunctionSigTypeParam { is_ty }
                    })
                    .collect();

                Some(ast::TypeList::new(items, type_params.span().clone()))
            },
            None => None,
        };

        Self {
            return_ty,
            params,
            type_params,
        }
    }

    pub fn from_decl(decl: FunctionDecl) -> Self {
        let return_ty = decl.return_ty;

        let param_sigs = decl
            .params
            .into_iter()
            .map(|p| FunctionSigParam::from_decl_param(p))
            .collect();
        
        let sig = Self::new(return_ty, param_sigs, decl.name.type_params);

        sig
    }

    pub fn visit_types_ref<Visitor>(&self, visitor: Visitor)
    where
        Visitor: Copy + Fn(&Type) 
    {
        self.return_ty.visit_types(visitor);

        for param in &self.params {
            param.ty.visit_types(visitor);
            visitor(&param.ty);
        }

        if let Some(type_params) = &self.type_params {
            for param in &type_params.items {
                param.is_ty.visit_types(visitor);
            }
        }
    }

    pub fn visit_types_mut<Visitor>(&mut self, visitor: Visitor)
    where
        Visitor: Copy + Fn(&mut Type)
    {
        self.return_ty.visit_types_mut(visitor);

        for param in &mut self.params {
            param.ty.visit_types_mut(visitor);
        }

        if let Some(type_params) = self.type_params.as_mut() {
            for param in &mut type_params.items {
                param.is_ty.visit_types_mut(visitor);
            }
        }
    }

    pub fn of_anonymous_func(func: &AnonymousFunctionDef) -> Rc<Self> {
        func.annotation
            .ty()
            .as_func()
            .cloned()
            .expect("anonymous function must have function type")
    }

    pub fn contains_generic_params(&self, ctx: &Context) -> bool {
        if self.return_ty.contains_unresolved_params(ctx) {
            return true;
        }
        
        self.params
            .iter()
            .any(|param| param.ty.contains_unresolved_params(ctx))
    }
    
    pub fn validate_generic_args(&self, args: &TypeArgList, ctx: &Context) -> GenericResult<()> {
        let Some(type_params) = &self.type_params else {
            return Err(GenericError::ArgsLenMismatch {
                target: GenericTarget::FunctionSig(self.clone()),
                actual: args.len(),
                expected: 0,
            })
        };
        
        if args.len() != type_params.len() {
            return Err(GenericError::ArgsLenMismatch {
                target: GenericTarget::FunctionSig(self.clone()),
                actual: args.len(),
                expected: type_params.len(),
            })
        }

        for pos in 0..type_params.len() {
            let ty_arg = &args.items[pos];
            let constraint_ty = &type_params[pos].is_ty;

            if !ty_arg.match_constraint(constraint_ty, ctx) {
                return Err(GenericError::ConstraintNotSatisfied {
                    is_not_ty: constraint_ty.clone(),
                    actual_ty: Some(ty_arg.clone()),
                });
            }
        }

        Ok(())
    }

    pub fn type_params_len(&self) -> usize {
        self.type_params.as_ref().map(|list| list.len()).unwrap_or(0)
    }

    pub fn apply_type_args(&self, ty_params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        let mut new_sig = self.clone();
        new_sig.visit_types_mut(|ty| {
            if let Some(arg) = ty_params.find_in_type_args(ty, args) {
                *ty = arg.clone();
            }
        });

        new_sig
    }

    // todo: need a deep implementation of this
    pub fn eq_as_impl(&self, other: &Self) -> bool {
        if self.type_params != other.type_params {
            return false;
        }
        
        if self.params.len() != other.params.len() {
            return false;
        }

        let mut inferred_self = None;

        for (own_param, other_param) in self.params.iter().zip(other.params.iter()) {
            if own_param.modifier != other_param.modifier {
                return false;
            }

            if own_param.ty != other_param.ty {
                if own_param.ty != Type::MethodSelf {
                    return false;
                }

                match &inferred_self {
                    None => {
                        inferred_self = Some(other_param.ty.clone())
                    },

                    Some(prev_inferred) => {
                        if *prev_inferred != other_param.ty {
                            return false;
                        }
                    }
                }
            }
        }

        if self.return_ty != other.return_ty {
            if self.return_ty != Type::MethodSelf {
                return false
            }
            
            if let Some(prev_inferred) = inferred_self {
                if prev_inferred != other.return_ty {
                    return false;
                }
            }
        }

        true
    }

    /// replace all `Self`-typed args with `self_ty`
    // todo: need a deep implementation of this
    pub fn with_self(&self, self_ty: &Type) -> Self {
        let mut new_sig = self.clone();
        new_sig.visit_types_mut(|ty| {
            if matches!(ty, Type::MethodSelf) {
                *ty = self_ty.clone();
            } 
        });
        
        new_sig
    }

    /// given that `self` is the sig of an interface method with one
    /// or more `Self`-typed arguments, find the expected self-type for
    pub fn self_ty_from_args<'arg>(&self, args: &'arg [Type]) -> Option<&'arg Type> {
        if args.len() != self.params.len() {
            return None;
        }

        let self_arg_pos = self
            .params
            .iter()
            .position(|arg| arg.ty == Type::MethodSelf)?;

        Some(&args[self_arg_pos])
    }

    /// given that `self` is the sig of an interface method,
    /// for what type does `impl_func` implement this method, if any?
    pub fn impl_ty<'func>(&self, impl_func: &'func Self) -> Option<&'func Type> {
        if self.params.len() != impl_func.params.len() {
            return None;
        }

        let self_type = if self.return_ty == Type::MethodSelf {
            &impl_func.return_ty
        } else {
            self.params
                .iter()
                .position(|param| param.ty == Type::MethodSelf)
                .map(|pos| &impl_func.params[pos].ty)?
        };

        // `Nothing` can't have interface impls
        if *self_type == Type::Nothing {
            return None;
        }

        let self_positions: Vec<_> = self
            .params
            .iter()
            .enumerate()
            .filter_map(|(pos, param)| {
                if param.ty == Type::MethodSelf {
                    Some(pos)
                } else {
                    None
                }
            })
            .collect();

        for pos in 0..self.params.len() {
            if self_positions.contains(&pos) {
                // self-typed params must all be the same type as either the
                // first such param, or the return type if it's self-typed too
                if impl_func.params[pos].ty != *self_type {
                    return None;
                }
            } else if impl_func.params[pos] != self.params[pos] {
                // non-self params must match exactly
                return None;
            }
        }

        Some(self_type)
    }

    /// when the contextual type hint means we're expecting a function value that matches
    /// the signature of the function, the expression evaluates to the function itself rather
    /// than a call to the function. it could never refer to the return value of the function,
    /// since it's impossible for a function to return a function with its own signature, which
    /// would be a recursive type
    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg_ty: &Type) -> bool {
        // if we expect a self-param (eg the `a` in the no-args call `a.B`), it can only be a
        // no-args call if there is exactly one param. 
        // if there's no self-param, there must be zero actual args
        let with_params_len = match self_arg_ty {
            Type::Nothing => 0,
            _ => 1,
        };

        if self.params.len() != with_params_len {
            return false;
        }

        match expect_ty {
            // does the expected type of this value match the full sig (after including self, if
            // applicable)? if so we aren't calling the function, we're referencing it
            Type::Function(expect_sig) => match self_arg_ty {
                Type::Nothing => **expect_sig != *self,

                ty => {
                    let actual_sig = self.with_self(ty);
                    **expect_sig != actual_sig
                }
            },

            _ => true,
        }
    }

    pub fn new_function_call(
        &self,
        target: Expr,
        span: Span,
        args: impl IntoIterator<Item = Expr>,
        args_span: Span,
        type_args: Option<TypeArgList>,
    ) -> FunctionCall {
        let func_val_annotation = match &self.return_ty {
            Type::Nothing => Value::Untyped(span),
            return_ty => Value::new_temp_val(return_ty.clone(), span),
        };

        FunctionCall {
            annotation: func_val_annotation,
            args: args.into_iter().collect(),
            args_span,
            target,
            type_args,
        }
    }

    pub fn new_no_args_method_call(&self,
        target: Expr,
        owning_type: Type,
        self_arg: Option<Expr>,
        type_args: Option<TypeArgList>,
    ) -> MethodCallNoArgs {
        let span = target.span().clone();
        let func_val_annotation = match &self.return_ty {
            Type::Nothing => Value::Untyped(span),
            return_ty => Value::new_temp_val(return_ty.clone(), span),
        };
        
        MethodCallNoArgs {
            annotation: func_val_annotation,
            self_arg,
            owning_type,
            target,
            type_args,
        }
    }
    
    fn type_param_name(pos: usize) -> String {
        format!("T{pos}")
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")?;
        
        if let Some(type_params) = &self.type_params {
            write!(f, "[")?;
            for i in 0..type_params.len() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", Self::type_param_name(i))?;
            }
            write!(f, "]")?;
        }

        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                if let Some(modifier) = &param.modifier {
                    write!(f, "{} ", modifier)?;
                }

                write!(f, "{}", param.ty)?;
            }
            write!(f, ")")?;
        }
        
        if self.return_ty != Type::Nothing {
            write!(f, ": {}", self.return_ty)?;
        }

        if let Some(type_params) = &self.type_params {
            if type_params.iter().any(|p| p.is_ty != Type::Any) {
                write!(f, " where ")?;

                let mut constraint_count = 0;
                for i in 0..type_params.len() {
                    if type_params[i].is_ty == Type::Any {
                        continue;
                    }

                    if constraint_count > 0 {
                        write!(f, "; ")?;
                    }

                    write!(f, "T{i} is {}", type_params[i].is_ty)?;
                    constraint_count += 1;
                }
            }
        }
        
        Ok(())
    }
}
