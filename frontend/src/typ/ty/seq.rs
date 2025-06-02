use crate::ast::{Access, FunctionDeclKind};
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::is_system_option_name;
use crate::typ::Context;
use crate::typ::NameError;
use crate::typ::Type;
use crate::Ident;
use std::sync::Arc;

pub const SEQUENCE_METHOD_NAME: &str = "Sequence";
pub const SEQUENCE_NEXT_METHOD_NAME: &str = "Next";

#[derive(Debug)]
pub enum TypeSequenceError {
    MethodNotFound,
    MethodNotAccessible(Type, Ident, Access),
    AmbiguousSequenceMethod(Vec<OverloadCandidate>),
    AmbiguousNextMethod(Vec<OverloadCandidate>),
    Error(NameError),
}

#[derive(Debug, Clone)]
pub struct TypeSequenceSupport {
    pub item_type: Type,
    pub sequence_type: Type,

    pub sequence_method_index: usize,
    pub sequence_method_decl: Arc<FunctionDecl>,

    pub item_next_method_index: usize,
    pub item_next_method_decl: Arc<FunctionDecl>,
}

impl From<NameError> for TypeSequenceError {
    fn from(err: NameError) -> Self {
        TypeSequenceError::Error(err)
    }
}

impl TypeSequenceSupport {
    /// sequence-compatible types can be used with sequence enumeration via duck typing. they don't
    /// need to implement any interfaces, but must have a method named `Sequence` that returns an
    /// object with the method `Next` of the signature `function: Option[T]` where T can be
    /// any type and will be used as the type of the item binding in the loop
    pub fn try_from_type(
        ty: &Type,
        ctx: &Context
    ) -> Result<Self, TypeSequenceError> {
        let (seq_method_index, seq_method) = find_unique_method_by_name(
            ty,
            SEQUENCE_METHOD_NAME,
            ctx,
            TypeSequenceError::AmbiguousSequenceMethod,
        )?;

        let sequence_ty = &seq_method.func_decl.result_ty;
        let (next_index, next_decl, item_ty) = Self::find_sequence_next_method(sequence_ty, ctx)?;

        Ok(TypeSequenceSupport {
            item_type: item_ty,
            sequence_type: sequence_ty.clone(),
            
            sequence_method_index: seq_method_index,
            sequence_method_decl: seq_method.func_decl,
            
            item_next_method_index: next_index,
            item_next_method_decl: next_decl.func_decl,
        })
    }

    fn find_sequence_next_method(
        ty: &Type,
        ctx: &Context
    ) -> Result<(usize, MethodDecl, Type), TypeSequenceError> {
        let (next_method_index, next_method) = find_unique_method_by_name(
            ty,
            SEQUENCE_NEXT_METHOD_NAME,
            ctx,
            TypeSequenceError::AmbiguousNextMethod,
        )?;

        match &next_method.func_decl.result_ty {
            Type::Variant(sym) if is_system_option_name(sym) => {
                let Some(item_ty) = sym.type_args
                    .as_ref()
                    .map(|args| &args.items[0]) else 
                {
                    return Err(TypeSequenceError::MethodNotFound);   
                };
                
                
                let item_ty = item_ty.clone();
                Ok((next_method_index, next_method, item_ty.into()))
            }
            
            // incompatible
            _ => {
                Err(TypeSequenceError::MethodNotFound)
            }
        }
    }
}

fn find_unique_method_by_name<ToAmbigErrFn>(
    ty: &Type,
    method_name: &str,
    ctx: &Context,
    to_ambig_err: ToAmbigErrFn
) -> Result<(usize, MethodDecl), TypeSequenceError> 
where
    ToAmbigErrFn: FnOnce(Vec<OverloadCandidate>) -> TypeSequenceError
{
    let mut methods: Vec<_> = ty
        .methods(ctx)?
        .into_iter()
        .enumerate()
        .collect();
    
    methods.retain(|(_index, m)| {
        let method_decl = &m.func_decl;
        
        if method_decl.name.type_params.is_some() {
            return false;
        }
        
        if method_decl.kind != FunctionDeclKind::Function {
            return false;
        }

        if method_decl.params.len() != 1 || method_decl.params[0].ty != *ty {
            return false;
        }

        if method_decl.name.ident.as_str() != method_name {
            return false;
        }
        
        true
    });

    // if all the candidates are inaccessible, report that as a separate error
    if methods.iter().any(|(_, m)| ty.get_current_access(ctx) < m.access) {
        let (_, m) = &methods[0];

        let method_ident = m.func_decl.name.ident.clone();
        return Err(TypeSequenceError::MethodNotAccessible(ty.clone(), method_ident, m.access))
    }
    
    methods.retain(|(_, m)| ty.get_current_access(ctx) >= m.access);

    if methods.is_empty() {
        Err(TypeSequenceError::MethodNotFound)
    } else if methods.len() > 1 {
        let overloads = methods
            .into_iter()
            .map(|(index, decl)| {
                OverloadCandidate::Method {
                    index,
                    decl,
                    self_ty: ty.clone(),
                    iface_ty: ty.clone(),
                }
            })
            .collect();
        
        Err(to_ambig_err(overloads))
    } else {
        let (index, method) = methods.into_iter().next().unwrap();
        Ok((index, method))
    }
}
