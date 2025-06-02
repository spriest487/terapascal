use crate::typ;
use crate::typ::{FunctionSig, TypeName};
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParam;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct GenericContext {
    items: Vec<ResolvedTypeArg>,
}

impl GenericContext {
    pub fn empty() -> Self {
        Self {
            items: Vec::new(),
        }
    }
    
    pub fn new(params: &TypeParamList, args: &TypeArgList) -> Self {
        assert_eq!(params.len(), args.len());
        
        let mut ctx = GenericContext::empty();
        
        for (param, arg) in params.iter().zip(args.iter()) {
            ctx.add(param.clone(), arg.ty().clone());
        }
        
        ctx
    }
    
    pub fn child_context(&self, params: &TypeParamList, args: &TypeArgList) -> Self {
        assert_eq!(params.len(), args.len());

        let params = params.clone().apply_type_args(self, self);        
        let args = args.clone().apply_type_args(self, self);

        let mut child = self.clone();
        for (param, arg) in params.items.into_iter().zip(args.items.into_iter()) {
            child.add(param.clone(), arg.ty().clone());
        }
        
        child
    }
    
    pub fn push(&mut self, params: &TypeParamList, args: &TypeArgList) {
        *self = self.child_context(params, args);
    }
    
    pub fn append(&mut self, other: &mut Self) {
        self.items.append(&mut other.items);
    }
    
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    
    pub fn add(&mut self, param: TypeParam, arg: Type) {
        self.items.push(ResolvedTypeArg {
            arg,
            param,
        })
    }
    
    pub fn apply_to_type(&self, ty: Type) -> Type {
        ty.apply_type_args(self, self)
    }

    pub fn apply_to_typename(&self, ty: TypeName) -> TypeName {
        ty.apply_type_args(self, self)
    }

    pub fn apply_to_sig(&self, sig: &FunctionSig) -> FunctionSig {
        sig.apply_type_args(self, self)
    }
    
    pub fn find_arg(&self, name: &str) -> Option<&Type> {
        self.items
            .iter()
            .find(|i| i.param.name.as_str() == name)
            .map(|i| &i.arg)
    }

    pub fn into_items(self) -> Vec<ResolvedTypeArg> {
        self.items
    }
    
    pub fn into_args(self) -> impl Iterator<Item=Type> {
        self.items
            .into_iter()
            .map(|i| i.arg)
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedTypeArg {
    pub param: typ::TypeParam,
    pub arg: typ::Type,
}

impl TypeArgResolver for GenericContext {
    fn get(&self, pos: usize) -> Option<&typ::Type> {
        self.items.get(pos).map(|item| &item.arg)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

impl TypeParamContainer for GenericContext {
    fn find_position(&self, name: &str) -> Option<usize> {
        self.items
            .iter()
            .position(|p| p.param.name.name.as_str() == name)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

impl fmt::Display for GenericContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut has_constraints = false;
        
        write!(f, "[")?;
        
        for i in 0..self.items.len() {
            if i > 0 {
                write!(f, ", ")?;
            }
            
            write!(f, "{}", self.items[i].param.name)?;
            has_constraints |= self.items[i].param.constraint.is_some();
            
            write!(f, " = {}", self.items[i].arg)?;
        }
        
        write!(f, "]")?;
        
        if has_constraints {
            let mut constraint_count = 0;
            write!(f, " where ")?;
            for item in &self.items {
                let Some(constraint) = &item.param.constraint else {
                    continue;
                };

                if constraint_count > 0 {
                    write!(f, ", ")?;
                }
                
                write!(f, "{}", constraint)?;
                
                constraint_count += 1;
            }
        }
        
        Ok(())
    }
}
