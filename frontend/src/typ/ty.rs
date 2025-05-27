mod specialize;
mod array;

#[cfg(test)]
mod test;

pub mod layout;
pub mod pattern;
pub mod primitive;
pub mod sig;
pub mod ty_param;
pub mod set;
pub mod seq;

pub use self::array::*;
pub use self::pattern::*;
pub use self::primitive::*;
pub use self::set::*;
pub use self::sig::*;
pub use self::specialize::*;
pub use self::ty_param::*;
use crate::ast;
use crate::ast::Access;
use crate::ast::ArrayTypeName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::ast::StructKind;
use crate::ast::TypeAnnotation;
use crate::ast::IFACE_METHOD_ACCESS;
use crate::typ::ast::FieldDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::SELF_TY_NAME;
use crate::typ::builtin_unit_path;
use crate::typ::Context;
use crate::typ::Def;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::NameResult;
use crate::typ::Symbol;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ANY_TYPE_NAME;
use crate::typ::NIL_NAME;
use crate::typ::NOTHING_TYPE_NAME;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::typ::builtin_span;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::Keyword;
use crate::Operator;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Type {
    Nothing,
    Nil,
    Primitive(Primitive),
    Pointer(Arc<Type>),
    Function(Arc<FunctionSig>),
    Record(Arc<Symbol>),
    Class(Arc<Symbol>),
    Interface(Arc<Symbol>),
    Variant(Arc<Symbol>),
    Array(Arc<ArrayType>),
    DynArray { element: Arc<Type> },
    MethodSelf,
    GenericParam(Arc<TypeParamType>),
    Weak(Arc<Type>),
    Any,
    Enum(Arc<IdentPath>),
    Set(Arc<SetType>),
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Type::Primitive(primitive)
    }
}

impl From<ArrayType> for Type {
    fn from(array_ty: ArrayType) -> Self {
        Type::Array(Arc::new(array_ty))
    }
}

impl Type {
    pub fn record(sym: impl Into<Symbol>) -> Self {
        Type::Record(Arc::new(sym.into()))
    }

    pub fn class(sym: impl Into<Symbol>) -> Self {
        Type::Class(Arc::new(sym.into()))
    }

    pub fn variant(sym: impl Into<Arc<Symbol>>) -> Self {
        Type::Variant(sym.into())
    }

    pub fn enumeration(sym: impl Into<IdentPath>) -> Self {
        Type::Enum(Arc::new(sym.into()))
    }

    pub fn set(set: impl Into<Arc<SetType>>) -> Self {
        Type::Set(set.into())
    }
    
    pub fn interface(name: impl Into<Symbol>) -> Self {
        Type::Interface(Arc::new(name.into()))
    }
    
    pub fn generic_param(name: Ident) -> Type {
        let ty_param_ty = TypeParamType {
            name,
            is_ty: Type::Any,
        };
        
        Type::GenericParam(Arc::new(ty_param_ty))
    }

    pub fn array(self, dim: usize) -> Self {
        Type::Array(Arc::new(ArrayType {
            element_ty: self,
            dim,
        }))
    }

    pub fn dyn_array(self) -> Self {
        Type::DynArray { element: Arc::new(self) }
    }

    pub fn generic_constrained_param(name: Ident, is_iface: Type) -> Type {
        let ty_param_ty = TypeParamType {
            name,
            is_ty: is_iface,
        };

        Type::GenericParam(Arc::new(ty_param_ty))
    }
    
    pub fn from_struct_type(sym: impl Into<Symbol>, kind: StructKind) -> Self {
        let sym = Arc::new(sym.into());
        
        match kind {
            StructKind::Class => Type::Class(sym),
            StructKind::Record => Type::Record(sym),
        }
    }
    
    // todo: this doesn't reflect the declared type exactly since it loses the "packed" qualifier
    // does "packed" actually need to be a separate struct kind or could it become a decl field?
    pub fn struct_kind(&self) -> Option<StructKind> {
        match self {
            Type::Record(..) => Some(StructKind::Record),
            Type::Class(..) => Some(StructKind::Class),
            _ => None,
        }
    }

    pub fn full_name(&self) -> Option<Cow<Symbol>> {
        fn builtin_sym(name: &str) -> Option<Cow<Symbol>> {
            Some(Cow::Owned(Symbol::from(builtin_unit_path(name))))
        }
        
        match self {
            Type::Nothing => builtin_sym(NOTHING_TYPE_NAME),
            Type::Any => builtin_sym(ANY_TYPE_NAME),
            Type::Primitive(p) => builtin_sym(p.name()),

            // Type::MethodSelf => Some(Cow::Owned(IdentPath::from(Ident::new(SELF_TY_NAME, builtin_span())))),

            Type::Interface(decl_name)
            | Type::Record(decl_name)
            | Type::Class(decl_name)
            | Type::Variant(decl_name) => Some(Cow::Borrowed(decl_name)),

            _ => None,
        }
    }
    
    pub fn full_path(&self) -> Option<Cow<IdentPath>> {
        match self {
            Type::Nothing => Some(Cow::Owned(builtin_unit_path(NOTHING_TYPE_NAME))),
            Type::Any => Some(Cow::Owned(builtin_unit_path(ANY_TYPE_NAME))),
            Type::Primitive(p) => Some(Cow::Owned(builtin_unit_path(p.name()))),
            
            // Type::MethodSelf => Some(Cow::Owned(IdentPath::from(Ident::new(SELF_TY_NAME, builtin_span())))),
            
            Type::Interface(decl_name)
            | Type::Record(decl_name) 
            | Type::Class(decl_name) 
            | Type::Variant(decl_name) => Some(Cow::Borrowed(&decl_name.full_path)),
            
            _ => None,
        }
    }

    pub fn is_by_ref(&self) -> bool {
        match self {
            Type::Nothing => false,
            Type::Nil => false,
            Type::Primitive(_) => false,
            Type::Pointer(_) => false,
            Type::Function(_) => false,
            Type::Record(_) => false,
            Type::Variant(_) => false,
            Type::Array { .. } => false,
            Type::GenericParam(_) => false,
            Type::Enum(..) => false,
            Type::Set(..) => false,

            Type::Class(_) => true,
            Type::Interface(_) => true,
            Type::DynArray { .. } => true,
            Type::MethodSelf => true,
            Type::Weak(..) => true,
            Type::Any => true,
        }
    }

    pub fn has_default(&self, ctx: &Context) -> NameResult<bool> {
        // in an unsafe context, we can assign a default value to any variable
        if ctx.allow_unsafe() {
            return Ok(true);
        }

        let result = match self {
            Type::Any
            | Type::Class(_)
            | Type::Interface(_)
            | Type::DynArray { .. }
            | Type::Function(_)
            | Type::Nothing
            | Type::GenericParam(_)
            | Type::Weak(..)
            | Type::Set(..)
            | Type::Enum(_) => false,

            | Type::Variant(variant_name) => {
                let def = ctx.instantiate_variant_def(&variant_name)?;
                let mut result = true;
                for case in &def.cases {
                    if let Some(data_ty) = &case.data_ty {
                        if !data_ty.has_default(ctx)? {
                            result = false;
                            break;
                        }
                    }
                }

                result
            }

            | Type::Record(..) => {
                let fields = self.fields(ctx)?;
                let mut result = true;
                for field in &fields {
                    if !field.ty.has_default(ctx)? {
                        result = false;
                        break;
                    }
                }
                
                result
            }

            Type::Array(array_ty) => array_ty.element_ty.has_default(ctx)?,

            Type::Nil => true,
            Type::Primitive(..) => true,
            Type::Pointer(..) => true,
            
            Type::MethodSelf => {
                let current_self_ty = ctx
                    .current_function_body_env()
                    .and_then(|env| env.self_ty.as_ref());
                
                if let Some(self_ty) = current_self_ty {
                    self_ty.has_default(ctx)?
                } else {
                    false
                }
            }
        };
        
        Ok(result)
    }

    pub fn of_decl(type_decl: &ast::TypeDeclItem<Value>, ctx: &Context) -> TypeResult<Self> {
        match type_decl {
            ast::TypeDeclItem::Struct(class)
            if class.kind == StructKind::Record => {
                Ok(Type::Record(Arc::new(class.name.clone())))
            },

            ast::TypeDeclItem::Struct(class) => {
                Ok(Type::Class(Arc::new(class.name.clone())))
            },

            ast::TypeDeclItem::Variant(variant) => {
                Ok(Type::Variant(variant.name.clone()))
            },

            ast::TypeDeclItem::Interface(iface) => {
                Ok(Type::interface(iface.name.clone()))
            },

            ast::TypeDeclItem::Enum(enum_decl) => {
                enum_decl.name.expect_no_type_params(InvalidTypeParamsDeclKind::Enum)?;
                
                Ok(Type::enumeration(enum_decl.name.full_path.clone()))
            },
            
            ast::TypeDeclItem::Set(set_decl) => {
                set_decl.name.expect_no_type_params(InvalidTypeParamsDeclKind::Set)?;

                let name = &set_decl.name.full_path;
                let set_decl = ctx
                    .find_type_def(name)
                    .and_then(|def| match def {
                        Def::Set(set_decl) => Some(set_decl),
                        _ => None,
                    })
                    .ok_or_else(|| TypeError::name_not_found(name.clone(), name.path_span()))?;
                
                let set_type = set_decl.to_set_type(ctx)?;

                Ok(Type::set(set_type))
            }

            ast::TypeDeclItem::Alias(alias) => {
                Ok((*alias.ty).clone())
            },
        }
    }

    pub fn find_data_member(&self, member: &Ident, ctx: &Context) -> NameResult<Option<FieldDecl>> {
        match self {
            Type::Class(class_name) | Type::Record(class_name) => {
                let struct_kind = self.struct_kind().unwrap();
                let def = ctx.instantiate_struct_def(class_name, struct_kind)?;

                Ok(def.find_field(member).cloned())
            },

            _ => Ok(None),
        }
    }

    pub fn get_field(&self, index: usize, ctx: &Context) -> NameResult<Option<FieldDecl>> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let struct_kind = self.struct_kind().unwrap();
                let class = ctx.instantiate_struct_def(class, struct_kind)?;
                let field = class.fields().nth(index).cloned();
                
                Ok(field)
            },

            _ => Ok(None),
        }
    }

    pub fn field_count(&self, ctx: &Context) -> NameResult<usize> {
        match self {
            Type::Record(class) | Type::Class(class) => {
                let struct_kind = self.struct_kind().unwrap();
                let class = ctx.find_struct_def(&class.full_path, struct_kind)?;
                Ok(class.fields().count())
            },

            _ => Ok(0),
        }
    }

    pub fn fields(&self, ctx: &Context) -> NameResult<Vec<FieldDecl>> {
        let mut members = Vec::new();
        for i in 0..self.field_count(ctx)? {
            let member = self.get_field(i, ctx)?.unwrap();

            members.push(member);
        }

        Ok(members)
    }

    pub fn is_generic_param(&self) -> bool {
        match self {
            Type::GenericParam(..) => true,
            _ => false,
        }
    }

    /// is this type, or any of the type parameters that it contains, a generic param type?
    /// e.g. in the sig `X[T](a: Box[T])`, the type of param `a` is "Box of type param 0".
    /// if this type appears in a context where the params already refer to specific types, 
    /// for example in the body of a function where this type refers to one of the function's type
    /// params, we ignore those types since they'll be real types when actually used
    pub fn contains_unresolved_params(&self, ctx: &Context) -> bool {
        if let Type::GenericParam(ty_param_ty) = self {
            let mut func_ty_params = ctx
                .current_function_body_env()
                .into_iter()
                .flat_map(|env| {
                    let func_ty_params = env.ty_params
                        .iter()
                        .flat_map(|params| params.iter());
                    
                    let enclosing_ty_params = env.self_ty
                        .iter()
                        .flat_map(|self_ty| self_ty
                            .type_params()
                            .into_iter()
                            .flat_map(|params| params.iter()));
                    
                    func_ty_params.chain(enclosing_ty_params)
                });

            return !func_ty_params.any(|param| param.name == ty_param_ty.name);
        }

        if let TypeArgsResult::Specialized(_, type_args) = &self.type_args() {
            return type_args.items.iter().any(|a| a.contains_unresolved_params(ctx));
        }

        match self {
            Type::Array(array_ty) => array_ty.element_ty.contains_unresolved_params(ctx),
            
            Type::DynArray { element, .. } => element.contains_unresolved_params(ctx),
            
            Type::Function(sig) => {
                sig.contains_generic_params(ctx)
            }

            _ => false,
        }
    }

    pub fn same_array_dim(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Array(a), Type::Array(b)) => a.dim == b.dim,

            _ => false,
        }
    }

    pub fn can_specialize_to(&self, other: &Self, ctx: &Context) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            (Type::GenericParam(param), ..) => {
                if param.is_ty == Type::Any {
                    return true;
                }

                ctx.is_implementation(other, &param.is_ty).unwrap_or(false)
            },

            (Type::Array(arr), Type::Array(other_arr)) => {
                arr.dim == other_arr.dim 
                    && arr.element_ty.can_specialize_to(&other_arr.element_ty, ctx)
            }

            (Type::DynArray { element }, Type::DynArray { element: other_element }) => {
                element.can_specialize_to(other_element, ctx)
            }

            (Type::Function(sig), Type::Function(other_sig)) => {
                if !sig.return_ty.can_specialize_to(&other_sig.return_ty, ctx)
                    || sig.params.len() == other_sig.params.len() {
                    return false;
                }
                
                sig.params
                    .iter()
                    .zip(other_sig.params.iter())
                    .all(|(p, other_p)| {
                        p.modifier == other_p.modifier
                            && p.ty.can_specialize_to(&other_p.ty, ctx)
                    })
            }
            
            _ => self.same_decl_type(other),
        }
    }

    pub fn same_decl_type(&self, other: &Self) -> bool {
        match (self.full_path(), other.full_path()) {
            (Some(path), Some(other_path)) => path == other_path,
            _ => false,
        }
    }

    /// get the type args this type is specialized with
    /// e.g. for the type `Box[Integer]`, the type list contains `Integer`
    /// returns `None` for non-generic types and unspecialized generic types
    pub fn type_args(&self) -> TypeArgsResult {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                match (&name.type_params, &name.type_args) {
                    (Some(type_params), None) => {
                        TypeArgsResult::Unspecialized(type_params)
                    }

                    (Some(type_params), Some(type_args)) => {
                        TypeArgsResult::Specialized(type_params, type_args)
                    }
                    
                    (None, None) => {
                        TypeArgsResult::NotGeneric
                    }
                    
                    (None, Some(..)) => unreachable!(),
                }
            },

            _ => TypeArgsResult::NotGeneric,
        }
    }

    pub fn type_params(&self) -> Option<&TypeParamList> {
        match self {
            Type::Variant(name) | Type::Class(name) | Type::Record(name) => {
                name.type_params.as_ref()
            },

            _ => None,
        }
    }

    pub fn array_element_ty(&self) -> Option<&Type> {
        match self {
            Type::DynArray { element } => Some(element),
            Type::Array(array_ty) => Some(&array_ty.element_ty),

            _ => None,
        }
    }

    pub fn is_strong_rc_reference(&self) -> bool {
        match self {
            Type::Class(..) => true,
            Type::Interface(..) => true,
            Type::Any => true,
            Type::DynArray { .. } => true,

            _ => false,
        }
    }

    /// Is this a value pointer type, not including primitive pointer types and RC pointers?
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(..) => true,
            _ => false,
        }
    }
    
    pub fn is_sized(&self, ctx: &Context) -> NameResult<bool> {
        let is_unsized = ctx.is_unsized_ty(self)?;
        Ok(!is_unsized)
    }
    
    pub fn expect_sized(&self, ctx: &Context, at: &Span) -> TypeResult<()> {
        let sized = self
            .is_sized(ctx)
            .map_err(|e| TypeError::from_name_err(e, at.clone()))?;

        if !sized {
            return Err(TypeError::InvalidUnsizedType {
                ty: self.clone(),
                span: at.clone(),
            });
        }
        
        Ok(())
    }

    pub fn deref_ty(&self) -> Option<&Type> {
        match self {
            Type::Pointer(ty) => Some(ty.as_ref()),
            _ => None,
        }
    }

    pub fn ptr(self) -> Self {
        Type::Pointer(Arc::new(self))
    }

    pub fn indirect_by(self, indirection: usize) -> Self {
        (0..indirection).fold(self, |ty, _| ty.ptr())
    }

    pub fn self_equatable(&self) -> bool {
        match self {
            | Type::Nothing
            | Type::Record(..)
            | Type::Variant(..)
            | Type::Function(..)
            | Type::GenericParam(..)
            | Type::Array(..)
            | Type::DynArray { .. }
            | Type::MethodSelf => false,

            | Type::Weak(..)
            | Type::Pointer(..)
            | Type::Primitive(..)
            | Type::Enum(..)
            | Type::Set(..)
            | Type::Nil
            | Type::Class(..)
            | Type::Any
            | Type::Interface(..) => true,
        }
    }

    pub fn equatable(&self, other: &Self, allow_unsafe: bool) -> bool {
        match (self, other) {
            (Type::Nil, Type::Pointer(..) | Type::Primitive(Primitive::Pointer)) => true,
            (Type::Pointer(..) | Type::Primitive(Primitive::Pointer), Type::Nil) => true,

            // weak and strong rc ptrs can be compared to null in an unsafe context
            (a, Type::Nil) | (Type::Nil, a) if a.is_strong_rc_reference() => allow_unsafe, 
            (Type::Weak(..), Type::Nil) | (Type::Nil, Type::Weak(..)) => allow_unsafe,
            
            (a, b) if a == b => a.self_equatable(),
            _ => false,
        }
    }

    pub fn self_orderable(&self) -> bool {
        match self {
            Type::Primitive(primitive) if primitive.is_numeric() => true,

            _ => false,
        }
    }

    pub fn arithmetic_op_result(&self, op: Operator, rhs: &Self) -> Option<Cow<Self>> {
        match (self, op, rhs) {
            // pointer-pointer subtraction - returns an isize offset
            (
                Type::Pointer(..) | Type::Primitive(Primitive::Pointer),
                Operator::Sub,
                Type::Pointer(..) | Type::Primitive(Primitive::Pointer),
            ) => {
                if self.deref_ty() != rhs.deref_ty() {
                    return None;
                }

                Some(Cow::Owned(Type::Primitive(Primitive::NativeInt)))
            }
            
            // pointer arithmetic:
            // - lhs is any pointer
            // - operator is +, - or any bitwise operator
            // - rhs is any integer type lte than the system pointer size
            (
                Type::Pointer(_) | Type::Primitive(Primitive::Pointer),
                Operator::Add
                | Operator::Sub
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Primitive(rhs_primitive),
            ) => {
                // can't offset untyped pointers
                if (op == Operator::Add || op == Operator::Sub) && self.deref_ty().is_none() {
                    return None;
                }
                
                let rhs_valid = rhs_primitive.is_integer() || rhs_primitive.is_pointer();
                if !rhs_valid || rhs_primitive.native_size() > Primitive::Pointer.native_size() {
                    return None;
                }

                Some(Cow::Borrowed(self))
            },

            // (typed) pointer arithmetic:
            // - lhs is any pointer
            // - rhs is +, - or any bitwise operator
            // - rhs is another pointer of the same type
            (
                Type::Pointer(_),
                Operator::Add
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Pointer(..),
            ) => {
                // can't offset untyped pointers
                if (op == Operator::Add || op == Operator::Sub) && self.deref_ty().is_none() {
                    return None;
                }

                if *self != *rhs {
                    return None;
                }

                Some(Cow::Owned(Type::Primitive(Primitive::NativeInt)))
            },

            // integer division is valid for two of the same primitive integer type
            (Type::Primitive(a), Operator::IDiv, Type::Primitive(b)) => {
                let a_valid = a.is_integer() || a.is_pointer();
                if !a_valid || *a != *b {
                    return None;
                }

                Some(Cow::Borrowed(self))
            },

            // real division is valid for two of the same primitive real type
            (Type::Primitive(a), Operator::FDiv, Type::Primitive(b)) => {
                if !a.is_real() || *a != *b {
                    return None;
                }

                Some(Cow::Borrowed(self))
            },

            // all maths ops except division are valid for primitives of the same type
            (
                Type::Primitive(a),
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Mod,
                Type::Primitive(b),
            ) => {
                if !a.is_numeric() || *a != *b {
                    return None;
                }
                
                Some(Cow::Borrowed(self))
            },

            // bitwise ops are valid for two identical unsigned primitive types
            (
                Type::Primitive(lhs),
                Operator::Shl
                | Operator::Shr
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret,
                Type::Primitive(rhs),
            ) if *lhs == *rhs => {
                if !lhs.is_integer() || lhs.is_signed() {
                    return None;
                }

                Some(Cow::Borrowed(self))
            },

            // bitwise ops are valid for sets with the same element type
            (
                Type::Set(lhs_set), 
                Operator::Shl
                | Operator::Shr
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::Caret, 
                Type::Set(rhs_set)
            ) => {
                if lhs_set.item_type != rhs_set.item_type {
                    return None;
                }

                Some(Cow::Borrowed(self))
            }

            _ => None,
        }
    }
    
    pub fn methods(&self, ctx: &Context) -> NameResult<Vec<MethodDecl>> {
        match self {
            Type::Interface(name) => {
                let iface_def = if name.is_unspecialized_generic() {
                    ctx.find_iface_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_iface_def(&name)?
                };

                let methods = iface_def
                    .methods
                    .iter()
                    .map(|m| MethodDecl { 
                        func_decl: m.decl.clone(),
                        access: IFACE_METHOD_ACCESS,
                    })
                    .collect();
                
                Ok(methods)
            },

            Type::Record(name) | Type::Class(name) => {
                let struct_kind = self.struct_kind().unwrap();
                let struct_def = if name.is_unspecialized_generic() {
                    ctx.find_struct_def(&name.full_path, struct_kind)?.clone()
                } else {
                    ctx.instantiate_struct_def(&name, struct_kind)?
                };

                let methods = struct_def 
                    .methods()
                    .map(|method| method.clone())
                    .collect();

                Ok(methods)
            }

            Type::Variant(name) => {
                let variant_def = if name.is_unspecialized_generic() {
                    ctx.find_variant_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&name)?
                };
                
                Ok(variant_def.methods.clone())
            }

            Type::Primitive(primitive) => {
                let methods = ctx
                    .get_primitive_methods(*primitive)
                    .values()
                    .cloned()
                    .collect();

                Ok(methods)
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.methods(ctx),
            //     None => Ok(Vec::new()),
            // }

            _ => Ok(Vec::new()),
        }
    }
    
    pub fn methods_at(&self,
        ctx: &Context,
        at: &Span
    ) -> TypeResult<Vec<MethodDecl>> {
        self
            .methods(ctx)
            .map_err(|err| {
                TypeError::from_name_err(err, at.clone())
            })
    }

    pub fn find_method_index(
        &self,
        name: &Ident,
        sig: &FunctionSig,
        ctx: &Context
    ) -> NameResult<Option<usize>> {
        match self {
            Type::Interface(iface_name) => {
                let iface_def = if iface_name.is_unspecialized_generic() {
                    ctx.find_iface_def(&iface_name.full_path)?.clone()
                } else {
                    ctx.instantiate_iface_def(&iface_name)?
                };

                let index = iface_def
                    .methods
                    .iter()
                    .enumerate()
                    .find_map(|(index, m)| {
                        if *m.ident() == *name && m.decl.sig().eq_as_impl(sig) {
                            Some(index)
                        } else {
                            None
                        }
                    });

                Ok(index)
            }

            Type::Record(type_name) | Type::Class(type_name) => {
                let struct_kind = self.struct_kind().unwrap();
                let struct_def = if type_name.is_unspecialized_generic() {
                    ctx.find_struct_def(&type_name.full_path, struct_kind)?.clone()
                } else {
                    ctx.instantiate_struct_def(&type_name, struct_kind)?
                };

                Ok(find_in_method_decls(name, sig, struct_def.methods.iter()))
            }

            Type::Variant(type_name) => {
                let variant_def = if type_name.is_unspecialized_generic() {
                    ctx.find_variant_def(&type_name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&type_name)?
                };

                Ok(find_in_method_decls(name, sig, variant_def.methods.iter()))
            }

            Type::Primitive(primitive) => {
                let methods = ctx.get_primitive_methods(*primitive);
                
                Ok(find_in_method_decls(name, sig, methods.values()))
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.find_method_index(name, sig, ctx),
            //     None => Ok(None),
            // }

            _ => Ok(None),
        }
    }

    pub fn get_method(&self, method_index: usize, ctx: &Context) -> NameResult<MethodDecl> {
        match self {
            Type::Interface(name) => {
                let iface_def = if name.is_unspecialized_generic() {
                    ctx.find_iface_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_iface_def(&name)?
                };

                let iface_method = iface_def.methods
                    .get(method_index)
                    .unwrap_or_else(|| panic!("invalid method index: {} in {}", method_index, name));
                
                Ok(MethodDecl {
                    func_decl: iface_method.decl.clone(),
                    access: IFACE_METHOD_ACCESS,
                })
            },

            Type::Record(name) | Type::Class(name) => {
                let struct_kind = self.struct_kind().unwrap();
                let struct_def = if name.is_unspecialized_generic() {
                    ctx.find_struct_def(&name.full_path, struct_kind)?.clone()
                } else {
                    ctx.instantiate_struct_def(&name, struct_kind)?
                };

                let method = struct_def.methods.get(method_index);
                Ok(method.cloned().expect("invalid method index"))
            }

            Type::Variant(name) => {
                let variant_def = if name.is_unspecialized_generic() {
                    ctx.find_variant_def(&name.full_path)?.clone()
                } else {
                    ctx.instantiate_variant_def(&name)?
                };

                let method = variant_def.methods.get(method_index);
                Ok(method.cloned().expect("invalid method index"))
            }

            Type::Primitive(primitive) => {
                let methods = ctx.get_primitive_methods(*primitive);
                let method = methods.values().nth(method_index);

                Ok(method.cloned().expect("invalid method index"))
            }

            // Type::GenericParam(param) => match &param.is_iface {
            //     Some(is_iface) => is_iface.get_method(method_index, ctx),
            //     None => panic!("invalid type for method: {self}"),
            // }

            _ => panic!("invalid type for method: {self}"),
        }
    }
    
    pub fn find_method(&self,
        method_ident: &Ident,
        sig: &FunctionSig,
        ctx: &Context
    ) -> NameResult<Option<(usize, MethodDecl)>> {
        match self.find_method_index(method_ident, sig, ctx)? {
            Some(index) => {
                let method = self.get_method(index, ctx)?;
                Ok(Some((index, method)))
            }

            None => {
                Ok(None)
            }
        }
    }
    
    pub fn get_current_access(&self, ctx: &Context) -> Access {
        match self.full_path() {
            Some(name) => ctx.get_access(name.as_ref()),
            
            None => Access::Public,
        }
    }
    
    fn implemented_ifaces_rec(
        base_tys: &[Type],
        all_ifaces: &mut HashSet<Type>,
        ctx: &Context
    ) -> NameResult<()> {
        for base_ty in base_tys {
            let Type::Interface(iface_name) = base_ty else {
               panic!("all base types must be interfaces")
            };

            let iface_def = ctx.instantiate_iface_def(iface_name)?;
            Self::implemented_ifaces_rec(&iface_def.supers, all_ifaces, ctx)?;

            all_ifaces.insert(base_ty.clone());
        }
        
        Ok(())
    }

    pub fn implemented_ifaces(&self, ctx: &Context) -> NameResult<HashSet<Type>> {
        match self {
            Type::GenericParam(param_ty) => match &param_ty.is_ty {
                Type::Any => Ok(HashSet::new()),

                is_iface => Ok(HashSet::from([is_iface.clone()])),
            },

            Type::Primitive(primitive) => {
                Ok(ctx.get_primitive_impls(*primitive).iter().cloned().collect())
            }

            Type::Record(name) | Type::Class(name) => {
                let struct_kind = self.struct_kind().unwrap();
                let def = ctx.instantiate_struct_def(name, struct_kind)?;
                
                let mut implements = HashSet::new();
                Self::implemented_ifaces_rec(&def.implements, &mut implements, ctx)?;
                
                Ok(implements)
            }
            
            Type::Variant(name) => {
                let def = ctx.instantiate_variant_def(name)?;
                
                let mut implements = HashSet::new();
                Self::implemented_ifaces_rec(&def.implements, &mut implements, ctx)?;

                Ok(implements)
            }

            _ => {
                Ok(HashSet::new())
            },
        }
    }
    
    pub fn implemented_ifaces_at(&self, ctx: &Context, at: &Span) -> TypeResult<HashSet<Type>> {
        self.implemented_ifaces(ctx)
            .map_err(|err| TypeError::from_name_err(err, at.clone()))
    }
    
    pub fn into_something(self) -> Option<Self> {
        match self {
            Type::Nothing => None,
            something => Some(something),
        }
    }

    pub fn expect_something(self, msg: &str) -> Self {
        match self {
            Type::Nothing => panic!("expected a type: {}", msg),
            x => x,
        }
    }

    pub fn element_ty(&self) -> Option<&Type> {
        match self {
            Type::Array(array_ty) => Some(&array_ty.element_ty),
            Type::DynArray { element } => Some(element.as_ref()),
            Type::Pointer(deref_ty) => Some(deref_ty.as_ref()),
            _ => None,
        }
    }

    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Nothing | Type::Nil => false,
            _ => true,
        }
    }

    pub fn as_primitive(&self) -> Option<Primitive> {
        match self {
            Type::Primitive(p) => Some(*p),
            _ => None,
        }
    }

    pub fn as_iface(&self) -> Option<&Symbol> {
        match self {
            Type::Interface(iface) => Some(iface),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Record(class) => Ok(&*class),
            other => Err(other),
        }
    }

    pub fn as_class(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Class(class) => Ok(&*class),
            other => Err(other),
        }
    }

    pub fn as_variant(&self) -> Result<&Symbol, &Self> {
        match self {
            Type::Variant(name) => Ok(&*name),
            other => Err(other),
        }
    }
    
    pub fn as_set(&self) -> Option<&Arc<SetType>> {
        match self {
            Type::Set(set_type) => Some(set_type),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Result<&Arc<FunctionSig>, &Self> {
        match self {
            Type::Function(sig) => Ok(sig),
            other => Err(other),
        }
    }

    pub fn match_constraint(&self, constraint_ty: &Type, ctx: &Context) -> bool {
        match constraint_ty {
            Type::Interface(..) => {
                // todo should we try to return an error here?
                ctx.is_implementation(self, constraint_ty).ok().unwrap_or(false)
            }

            // "Any" used as a constraint means all types, nothing to validate
            Type::Any => true,

            // nothing else is a valid constraint
            _ => false,
        }
    }

    pub fn specialize<'a, 'res>(
        &'a self,
        args: &'a TypeArgList,
        ctx: &Context,
    ) -> GenericResult<Cow<'res, Self>>
    where
        'a: 'res,
    {
        let specialized = match self {
            Type::Record(sym) => {
                let sym = sym.specialize(args, ctx)?;
                let record_ty = Type::Record(Arc::new(sym.into_owned()));

                Cow::Owned(record_ty)
            },

            Type::Class(sym) => {
                let sym = sym.specialize(args, ctx)?;
                let class_ty = Type::Class(Arc::new(sym.into_owned()));

                Cow::Owned(class_ty)
            },

            Type::Variant(variant) => {
                let sym = variant.specialize(args, ctx)?;
                let variant_ty = Type::Variant(Arc::new(sym.into_owned()));

                Cow::Owned(variant_ty)
            },
            
            Type::Interface(iface) => {
                let sym = iface.specialize(args, ctx)?;
                let iface_ty = Type::Interface(Arc::new(sym.into_owned()));

                Cow::Owned(iface_ty)
            }

            not_parameterized => {
                return Err(GenericError::ArgsLenMismatch {
                    target: GenericTarget::Type(not_parameterized.clone()),
                    actual: args.len(),
                    expected: 0,
                })
            },
        };

        Ok(specialized)
    }

    pub fn visit_generics<Visitor>(mut self, visitor: Visitor) -> Self
    where
        Visitor: Fn(TypeParamType) -> TypeParamType + Copy
    {
        self.visit_types_mut(|ty| {
            if let Type::GenericParam(type_param) = ty {
                *type_param = Arc::new(visitor((**type_param).clone()));
            }
        });

        self
    }

    pub fn visit_types<Visitor>(&self, visitor: Visitor)
    where
        Visitor: Copy + Fn(&Type),
    {        
        match self {
            Type::Record(sym) 
            | Type::Class(sym)
            | Type::Variant(sym) => {
                sym.visit_types_ref(visitor);
            },
            
            // todo: update this for generic interfaces decls
            Type::Interface(..) => {}, 

            Type::Array(array_ty) => {
                array_ty.element_ty.visit_types(visitor);
            },

            Type::DynArray { element } => {
                element.visit_types(visitor);
            },

            Type::Function(sig) => {
                sig.visit_types_ref(visitor);
            },

            Type::Pointer(deref) => {
                deref.visit_types(visitor);
            }

            Type::Weak(weak_ty) => {
                weak_ty.visit_types(visitor);
            }

            | Type::GenericParam(param_ty) => {
                param_ty.is_ty.visit_types(visitor);
            }

            | Type::Enum(..)
            | Type::Set(..)
            | Type::Nothing
            | Type::Nil
            | Type::Primitive(_)
            | Type::MethodSelf
            | Type::Any => {},
        }

        visitor(self);
    }
    
    pub fn visit_types_mut<Visitor>(&mut self, visitor: Visitor) 
    where
        Visitor: Copy + Fn(&mut Type),
    {
        match self {
            Type::Record(sym)
            | Type::Class(sym)
            | Type::Variant(sym) => {
                let mut new_sym = (**sym).clone();
                new_sym.visit_types_mut(visitor);
                *sym = Arc::new(new_sym)
            },

            // todo: update this for generic interfaces decls
            Type::Interface(..) => {}

            Type::Array(array_ty) => {
                let mut element_ty = array_ty.element_ty.clone();
                element_ty.visit_types_mut(visitor);

                *array_ty = Arc::new(ArrayType {
                    element_ty,
                    dim: array_ty.dim,
                });
            },

            Type::DynArray { element } => {
                let mut new_element = (**element).clone();
                new_element.visit_types_mut(visitor);

                *element = Arc::new(new_element);
            },

            Type::Function(sig) => {
                let mut new_sig = (**sig).clone();
                new_sig.visit_types_mut(visitor);
                *sig = Arc::new(new_sig);
            },

            Type::Pointer(deref) => {
                let mut new_deref = (**deref).clone();
                new_deref.visit_types_mut(visitor);
                *deref = Arc::new(new_deref);
            }

            Type::Weak(weak_ty) => {
                let mut new_deref = (**weak_ty).clone();
                new_deref.visit_types_mut(visitor);
                *weak_ty = Arc::new(new_deref);
            }
            
            Type::GenericParam(param_ty) => {
                let mut new_param_ty = (**param_ty).clone();
                new_param_ty.is_ty.visit_types_mut(visitor);
                *param_ty = Arc::new(new_param_ty);
            }
            
            Type::Nothing
            | Type::Enum(..)
            | Type::Set(..)
            | Type::Nil
            | Type::Primitive(_)
            | Type::MethodSelf
            | Type::Any => {},
        }

        visitor(self);
    }

    pub fn kind_description(&self) -> String {
        match self {
            Type::Nothing => "nothing".to_string(),
            Type::Nil => "nil".to_string(),
            Type::Primitive(..) => "primitive".to_string(),
            Type::Pointer(ty) => format!("pointer to {}", ty.kind_description()),
            Type::Function(..) => "function".to_string(),
            Type::Record(..) => "record".to_string(),
            Type::Class(..) => "class".to_string(),
            Type::Interface(..) => "interface".to_string(),
            Type::Variant(..) => "variant".to_string(),
            Type::Array(array_ty) => format!("array of {}", array_ty.element_ty.kind_description()),
            Type::DynArray { element } => format!("dynamic array of {}", element.kind_description()), 
            Type::MethodSelf => "self".to_string(),
            Type::GenericParam(..) => "generic argument".to_string(),
            Type::Weak(..) => "weak reference".to_string(),
            Type::Any => "any reference".to_string(),
            Type::Enum(..) => "enumeration".to_string(),
            Type::Set(..) => "set".to_string(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => write!(f, "{NIL_NAME}"),

            Type::Class(name) 
            | Type::Record(name)  => write!(f, "{}", name),

            Type::Enum(name)  => write!(f, "{}", name),

            Type::Set(set) => write!(f, "{}", set),

            Type::Weak(weak_ty) => write!(f, "{} {}", Keyword::Weak, weak_ty),
            Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
            Type::Any => write!(f, "{ANY_TYPE_NAME}"),

            Type::Interface(iface) => write!(f, "{}", iface),
            Type::Array(array_ty) => write!(f, "{}", array_ty),
            Type::DynArray { element } => write!(f, "array of {}", element),
            Type::GenericParam(ident) => write!(f, "{}", ident),
            Type::Nothing => write!(f, "{NOTHING_TYPE_NAME}"),
            Type::Primitive(p) => write!(f, "{}.{}", SYSTEM_UNIT_NAME, p.name()),
            Type::Variant(variant) => write!(f, "{}", variant),
            Type::MethodSelf => write!(f, "{SELF_TY_NAME}"),
            Type::Function(sig) => write!(f, "{}", sig),
        }
    }
}

impl TypeAnnotation for Type {
    fn is_known(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeMemberRef<'ty> {
    pub ident: &'ty Ident,
    pub ty: &'ty Type,
}

pub fn typecheck_type_path(path: &ast::TypePath, ctx: &mut Context) -> TypeResult<Type> {
    let (_, ty) = ctx
        .find_type(&path.name)
        .map_err(|err| TypeError::from_name_err(err, path.span.clone()))?;

    let ty = ty.clone();

    // validate type params, it's an error to write a path with mismatched type params
    let expect_params = ty.type_params().cloned();

    let params_match = match (&expect_params, &path.type_params) {
        (Some(expect), Some(actual)) => {
            if expect.len() != actual.len() {
                false
            } else {
                expect
                    .iter()
                    .zip(actual.iter())
                    .all(|(param, actual_name)| param.name == *actual_name)
            }
        }
        (None, None) => true,
        _ => false,
    };

    if !params_match {
        let err = GenericError::ParamMismatch {
            target: GenericTarget::Name(path.name.clone()),
            expected: expect_params,
            actual: path.type_params.clone(),
        };

        return Err(TypeError::from_generic_err(err, path.span.clone()));
    }
    
    Ok(ty)
}


pub fn typecheck_type(ty: &ast::TypeName, ctx: &mut Context) -> TypeResult<Type> {
    match ty {
        ast::TypeName::Ident(IdentTypeName {
            ident,
            indirection,
            type_args,
            span,
        }) => {
            let (_, raw_ty) = ctx
                .find_type(ident)
                .map_err(|err| TypeError::NameError {
                    err,
                    span: ty.span().clone(),
                })?;

            let raw_ty = raw_ty.clone();

            let ty = match type_args {
                Some(type_args) => {
                    let mut checked_type_arg_items = Vec::new();
                    for arg in &type_args.items {
                        let arg_ty = typecheck_type(arg, ctx)?;
                        checked_type_arg_items.push(arg_ty);
                    }

                    let type_args_span = type_args.span().clone();
                    let checked_type_args = TypeArgList::new(checked_type_arg_items, type_args_span);

                    Type::specialize(&raw_ty, &checked_type_args, ctx)
                        .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                        .into_owned()
                },

                None if raw_ty.is_unspecialized_generic() => {
                    return Err(TypeError::from_generic_err(GenericError::IllegalUnspecialized {
                        ty: raw_ty,
                    }, ty.span().clone()));
                }

                None => {
                    // eprintln!("using RAW: {raw_ty:?}");
                    raw_ty.clone()
                },
            };

            Ok(ty.indirect_by(*indirection))
        },

        ast::TypeName::Array(ArrayTypeName { element, dim, .. }) => {
            typecheck_array_type(element, dim, ctx)
        },

        ast::TypeName::Function(func_ty_name) => {
            let return_ty = match &func_ty_name.return_ty {
                Some(return_ty) => typecheck_type(return_ty, ctx)?,
                None => Type::Nothing,
            };

            let mut params = Vec::new();
            for param in &func_ty_name.params {
                let param_ty = typecheck_type(&param.ty, ctx)?;

                params.push(FunctionSigParam {
                    ty: param_ty,
                    modifier: param.modifier.clone(),
                });
            }

            let sig = FunctionSig::new(return_ty, params, None);
            let mut ty = Type::Function(Arc::new(sig));
            for _ in 0..func_ty_name.indirection {
                ty = ty.ptr();
            }

            Ok(ty)
        },
        
        ast::TypeName::Weak(weak_type, ..) => {
            let weak_type = typecheck_type(weak_type, ctx)?;
            if !weak_type.is_strong_rc_reference() {
                return Err(TypeError::InvalidWeakType {
                    ty: weak_type,
                    span: ty.span().clone(),
                });
            }
            
            Ok(Type::Weak(Arc::new(weak_type)))
        },

        ast::TypeName::Unspecified(_) => unreachable!("trying to resolve unknown type"),
    }
}

impl Specializable for Type {
    type GenericID = IdentPath;

    fn is_unspecialized_generic(&self) -> bool {
        if let Some(full_name) = self.full_name() {
            return full_name.is_unspecialized_generic();
        }

        match self {
            Type::Array(array_ty) => array_ty.element_ty.is_unspecialized_generic(),

            Type::DynArray { element } => element.is_unspecialized_generic(),

            Type::Function(sig) => {
                sig.return_ty.is_unspecialized_generic()
                    || sig.params.iter().any(|p| p.ty.is_unspecialized_generic())
            },

            _ => false,
        }
    }

    fn name(&self) -> Cow<IdentPath> {
        self.full_path()
            .expect("only types with full paths can be specialized")
    }

    /// Apply type a given set of type arguments to this type, with a parameter list indicating
    /// which parameters we're providing matching arguments for.
    /// e.g. in the method `Class[A].Method[B](a: A, b: B)`, there are two separate parameter
    /// lists which a provided arg list of length 1 could satisfy
    fn apply_type_args(mut self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        // callers should already have checked this
        assert_eq!(params.len(), args.len(), "apply_type_args: params and args counts did not match");
        
        self.visit_types_mut(|ty| {
            if let Some(result) = params.find_in_type_args(ty, args) {
                *ty = result.clone();
            }
        });

        self
    }
}

fn find_in_method_decls<'it, MethodsIter>(
    name: &Ident,
    sig: &FunctionSig,
    methods: MethodsIter
) -> Option<usize>
where
    MethodsIter: Iterator<Item=&'it MethodDecl>
{
    methods
        .enumerate()
        .find_map(|(index, m)| {
            if *m.func_decl.ident() == *name && m.func_decl.sig().eq_as_impl(sig) {
                Some(index)
            } else {
                None
            }
        })
}

pub fn string_type(ctx: &mut Context) -> TypeResult<Type> {
    let span = builtin_span();
    let ns = IdentPath::from(Ident::new("System", span.clone()));
    let str_class_name = ast::TypeName::Ident(IdentTypeName {
        ident: ns.child(Ident::new("String", span.clone())),
        indirection: 0,
        type_args: None,
        span,
    });

    typecheck_type(&str_class_name, ctx)
}
