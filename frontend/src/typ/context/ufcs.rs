#[cfg(test)]
mod test;

use crate::ast::Ident;
use crate::ast::Visibility;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::NameResult;
use crate::typ::Symbol;
use crate::typ::Type;
use std::fmt;
use std::fmt::Formatter;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub enum InstanceMethod {
    FreeFunction {
        /// fully-qualified function name, starting with the namespace the function is declared in
        func_name: Symbol,
        visibility: Visibility,
        
        decl: Arc<FunctionDecl>,
    },
    Method {
        self_ty: Type,
        iface_ty: Type,

        index: usize,
        method: MethodDecl,
    },
}

impl InstanceMethod {
    pub fn ident(&self) -> &Ident {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => func_name.ident(),
            InstanceMethod::Method { method, .. } => &method.func_decl.name.ident,
        }
    }
}

impl fmt::Display for InstanceMethod {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => {
                write!(f, "function {}", func_name)
            },

            InstanceMethod::Method { self_ty, method, .. } => {
                write!(f, "method {}.{}", self_ty, method.func_decl.name.ident )
            }
        }
        
    }
}

/// an instance method is an interface impl method for `ty_def` that takes Self as the first argument
/// OR a virtual method of the interface type referenced by `ty_def`
/// OR any free function taking `ty_def` as its first parameter (UFCS syntax)
/// OR any free function taking a generic type, or a parameterized type into which `ty_def` can
///    be substituted, as its first parameter (generic UFCS)
pub fn find_instance_methods_of(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();

    let mut free_functions = find_ufcs_free_functions(ty, ctx);
    methods.append(&mut free_functions);
    
    let mut ufcs_methods = find_ufcs_methods(ty, ctx)?;
    methods.append(&mut ufcs_methods);

    Ok(methods)
}

fn find_ufcs_free_functions(ty: &Type, ctx: &Context) -> Vec<InstanceMethod> {
    // the namespaces we look for UFCS methods in - the current NS and any used NSs
    let current_ns = ctx.namespace();
    let current_path = ctx.scopes.current_path();

    let mut search_namespaces = vec![&current_ns];
    search_namespaces.extend(current_path.all_used_namespaces());

    let mut methods = Vec::new();
    
    // eprintln!("members of {}: current scopes: {}", ty, ctx.scopes);

    ctx.scopes.visit_visible(|decl_path, decl| {        
        // ignore decls that aren't members of one of the search namespaces
        if !search_namespaces.iter().any(|search_ns| search_ns.is_parent_of(decl_path)) {
            return;
        }

        let overloads = match decl {
            Decl::Function { overloads, .. } => overloads,
            _ => return,
        };
        
        for overload in overloads {
            // ignore decls that can't possibly be called via UFCS since they have 0 params
            if overload.decl().params.is_empty() {
                continue;
            }

            let self_param = &overload.decl().params[0];

            if self_param.ty.can_specialize_to(ty, ctx) {
                let func_name = Symbol::from(decl_path.clone())
                    .with_ty_params(overload.decl().name.type_params.clone());

                methods.push(InstanceMethod::FreeFunction {
                    func_name,
                    decl: overload.decl().clone(),
                    visibility: overload.visiblity(),
                })
            }
        }
    });

    methods
}

fn find_ufcs_methods(self_ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    // if the type of the self-argument is a generic parameter with a constraint interface,
    // we can call methods through the interface, but we can't get a method index for it so we 
    // need to call it through the interface instead
    let (ty_methods, iface_ty) = match self_ty {
        Type::GenericParam(p) => {
            match &p.is_ty {
                Type::Any => return Ok(Vec::new()),

                is_iface => {
                    (is_iface.methods(ctx)?, is_iface.clone())
                }
            }
        }
        
        ty => {
            (ty.methods(ctx)?, ty.clone())
        },
    };

    // eprintln!("{} has methods:", ty);
    let instance_methods = ty_methods
        .into_iter()
        .enumerate()
        .filter_map(|(index, method)| {
            // eprintln!(" - {}", method.name);
            Some(InstanceMethod::Method {
                self_ty: self_ty.clone(),
                iface_ty: iface_ty.clone(),
                method: method.clone(),
                index,
            })
        })
        .collect();
    
    Ok(instance_methods)
}
