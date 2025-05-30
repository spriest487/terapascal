pub mod builtin;
pub mod scope;
pub mod value_kind;

mod decl;
mod def;
mod env;
mod result;
mod ufcs;
mod generic;

pub use self::builtin::*;
pub use self::decl::*;
pub use self::def::*;
pub use self::env::*;
pub use self::generic::*;
pub use self::result::*;
pub use self::scope::*;
pub use self::ufcs::InstanceMethod;
pub use self::value_kind::*;
use crate::ast as syn;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Path;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::ast::IFACE_METHOD_ACCESS;
use crate::ast::{Access, MethodOwner};
use crate::typ::ast::EnumDecl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::Literal;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::SetDecl;
use crate::typ::ast::StructDef;
use crate::typ::ast::VariantDef;
use crate::typ::ast::SELF_TY_NAME;
use crate::typ::specialize_by_return_ty;
use crate::typ::specialize_iface_def;
use crate::typ::specialize_struct_def;
use crate::typ::specialize_variant_def;
use crate::typ::FunctionSig;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeParamList;
use crate::typ::TypeParamType;
use crate::typ::TypeResult;
use terapascal_common::span::*;
use linked_hash_map::LinkedHashMap;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub ty: Type,
    pub kind: ValueKind,
    pub def: Option<Ident>,
}

#[derive(Clone, Debug)]
pub enum InstanceMember {
    Field {
        ty: Type,
        access: Access,
    },
    Method {
        iface_ty: Type,
        self_ty: Type,
        method: MethodDecl,
    },
    UFCSCall {
        func_name: Symbol,
        visibility: Visibility,
        decl: Rc<FunctionDecl>,
    },
    Overloaded {
        candidates: Vec<OverloadCandidate>,
    },
}

#[derive(Clone, Debug)]
pub enum TypeMember {
    Method(MethodGroupMember),
    MethodGroup(Vec<MethodGroupMember>),
}

impl TypeMember {
    pub fn from_method_members(mut members: Vec<MethodGroupMember>) -> TypeMember {
        if members.len() == 1 {
            let single_method = members.remove(0);
            TypeMember::Method(single_method)
        } else {
            TypeMember::MethodGroup(members)
        }
    }
}

#[derive(Clone, Debug)]
pub struct MethodGroupMember {
    pub iface_ty: Type,
    pub method: MethodDecl,
    pub index: usize,
}

impl TypeMember {
    pub fn access(&self) -> Access {
        match self {
            TypeMember::Method(member) => member.method.access,

            TypeMember::MethodGroup(members) => {
                members
                    .iter()
                    .map(|m| m.method.access)
                    .max()
                    .unwrap_or(Access::Public)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct MethodKey {
    pub name: Ident,
    pub sig: Rc<FunctionSig>,
}

#[derive(Clone, Debug, PartialEq)]
struct MethodCollection {
    methods: HashMap<MethodKey, Option<Rc<FunctionDef>>>,
}

impl MethodCollection {
    fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    // span for the whole module, should be position 0 of the primary source file
    module_span: Span,

    next_scope_id: ScopeID,
    scopes: ScopeStack,

    // builtin methods declarations for primitive types that can't be declared normally in code
    primitive_methods: HashMap<Primitive, LinkedHashMap<Ident, MethodDecl>>,

    // all primitives currently implement the same set of non-generic interfaces, so we can
    // just use this same list for all of them
    primitive_implements: Vec<Type>,

    defs: HashMap<IdentPath, HashMap<DefKey, Def>>,
    method_defs: HashMap<Type, MethodCollection>,

    loop_stack: Vec<Span>,
}

impl Context {
    pub fn root(module_span: Span) -> Self {
        let mut root_ctx = Self {
            module_span: module_span.clone(),

            scopes: ScopeStack::new(Scope::new(ScopeID(0), Environment::Global)),
            next_scope_id: ScopeID(1),

            defs: Default::default(),

            method_defs: Default::default(),

            loop_stack: Default::default(),

            primitive_methods: HashMap::new(),
            primitive_implements: Vec::new(),
        };

        let system_path = IdentPath::new(builtin_ident(SYSTEM_UNIT_NAME), []);
        root_ctx
            .unit_scope(system_path, |ctx| {
                let builtin_ifaces = [
                    builtin_displayable_iface(),
                    builtin_comparable_iface(),
                ];

                for builtin_iface in builtin_ifaces {
                    ctx.declare_iface(Rc::new(builtin_iface), Visibility::Interface)
                        .expect("builtin interface decl must not fail");
                }

                declare_builtin_ty(ctx, NOTHING_TYPE_NAME, Type::Nothing, false, false)
                    .expect("builtin type decl must not fail");
                declare_builtin_ty(ctx, ANY_TYPE_NAME, Type::Any, false, false)
                    .expect("builtin type decl must not fail");
                
                let typeinfo_ty = Type::class(builtin_typeinfo_name());
                declare_builtin_ty(ctx, TYPEINFO_TYPE_NAME, typeinfo_ty, false, false)
                    .expect("typeinfo type decl must not fail");

                for primitive in &Primitive::ALL {
                    let primitive_ty = Type::Primitive(*primitive);
                    let methods =
                        declare_builtin_ty(ctx, primitive.name(), primitive_ty, true, true)
                            .expect("primitive type decl must not fail");

                    ctx.primitive_methods.insert(*primitive, methods);
                }

                Ok(())
            })
            .expect("builtin unit definition must not fail");

        root_ctx
            .primitive_implements
            .push(Type::interface(builtin_displayable_name().full_path));
        root_ctx
            .primitive_implements
            .push(Type::interface(builtin_comparable_name().full_path));

        root_ctx
    }

    pub fn module_span(&self) -> &Span {
        &self.module_span
    }

    pub fn push_scope(&mut self, env: impl Into<Environment>) -> ScopeID {
        let env = env.into();

        let new_id = self.next_scope_id;
        self.next_scope_id = ScopeID(self.next_scope_id.0 + 1);

        self.scopes.push_scope(Scope::new(new_id, env));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeID) -> Scope {
        assert_ne!(ScopeID(0), id, "can't pop the root scope");

        loop {
            let popped = self.scopes.pop_scope();

            if popped.id() == id {
                break popped;
            }
        }
    }

    pub fn scope<F, T>(&mut self, env: impl Into<Environment>, f: F) -> TypeResult<T>
    where
        F: FnOnce(&mut Context) -> TypeResult<T>,
    {
        let scope_id = self.push_scope(env);

        let result = f(self);

        self.pop_scope(scope_id);

        result
    }

    pub fn unit_scope<T, F>(&mut self, unit_path: IdentPath, f: F) -> TypeResult<T>
    where
        F: FnOnce(&mut Context) -> TypeResult<T>,
    {
        let path_len = unit_path.as_slice().len();
        let mut path_parts = unit_path.into_vec();

        let mut unit_scopes = Vec::with_capacity(path_len);
        let mut part_path = Vec::with_capacity(path_len);

        path_parts.reverse();

        for _ in 0..path_len {
            let part = path_parts.pop().unwrap();
            let part_span = part.span.clone();
            part_path.push(part.clone());

            let part_ns = IdentPath::from_parts(part_path.clone());

            let current_scope = self.scopes.current_mut();
            match current_scope.remove_member(part_ns.last()) {
                None => {
                    // this part of the namespace is new, add a new scope for it
                    let scope = self.push_scope(Environment::Namespace { 
                        namespace: part_ns 
                    });
                    unit_scopes.push(scope);
                },

                Some(ScopeMember::Scope(existing_scope)) => {
                    // this is a previously declared namespace e.g. we are trying to define unit
                    // A.B.C and A.B has been previously declared - we take B out of the scope
                    // temporarily and make it active again. it'll be returned to its parent
                    // scope as normal when we pop it
                    unit_scopes.push(existing_scope.id());
                    self.scopes.push_scope(existing_scope);
                },

                Some(ScopeMember::Decl(decl)) => {
                    // we are trying to declare namespace A.B.C but A.B refers to something else that
                    // isn't a namespace
                    let err = NameError::Unexpected {
                        ident: part_ns.clone(),
                        actual: Named::from(decl.clone()),
                        expected: ExpectedKind::Namespace,
                    };

                    // restore the previous state
                    current_scope
                        .try_add_member(&part, ScopeMember::Decl(decl))
                        .unwrap();
                    for unit_scope in unit_scopes.into_iter().rev() {
                        self.pop_scope(unit_scope);
                    }

                    return Err(TypeError::from_name_err(err, part_span));
                },
            }
        }

        let result = f(self);

        for unit_scope in unit_scopes.into_iter().rev() {
            self.pop_scope(unit_scope);
        }

        result
    }

    pub fn push_loop(&mut self, at: Span) {
        self.loop_stack.push(at);
    }

    pub fn pop_loop(&mut self) -> Span {
        self.loop_stack
            .pop()
            .expect("can't pop loop stack when not in a loop")
    }

    pub fn in_loop(&self) -> Option<&Span> {
        self.loop_stack.last()
    }

    pub fn use_namespace(&mut self, unit: &IdentPath) {
        let mut current_path = self.scopes.current_path_mut();
        current_path.top().add_use_namespace(unit);
    }

    pub fn find_decl(&self, name: &Ident) -> Option<&Ident> {
        match self.find_name(name)? {
            ScopeMemberRef::Decl { key, .. } => Some(key),
            _ => None,
        }
    }

    pub fn find_name(&self, name: &Ident) -> Option<ScopeMemberRef> {
        self.find_path(&IdentPath::from_parts([name.clone()]))
    }

    pub fn find_path(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        // start by assuming any path we are searching for might be relative
        self.find_path_rec(path, true)
    }

    fn find_path_rec(&self, path: &IdentPath, path_is_relative: bool) -> Option<ScopeMemberRef> {
        match self.scopes.resolve_path(path) {
            // found an alias - resolve using its real name
            Some(ScopeMemberRef::Decl {
                value: Decl::Alias(aliased),
                ..
            }) => self.find_path(aliased),

            // matches a decl
            decl_ref @ Some(ScopeMemberRef::Decl { .. }) => {
                decl_ref
            },

            // matches a scope - does any decl match from a used unit though?
            scope_ref @ Some(ScopeMemberRef::Scope { .. }) => {
                if path_is_relative {
                    // always resolve to a decl if one matches in any using, rather than a scope
                    // even if the scope with this name is in this unit
                    match self.find_path_in_used_units(path) {
                        used_decl_ref @ Some(ScopeMemberRef::Decl { .. }) => used_decl_ref,
                        None => scope_ref,
                        used_scope_ref @ Some(ScopeMemberRef::Scope { .. }) => used_scope_ref,
                    }
                } else {
                    scope_ref
                }
            },

            // nothing matched in this scope, maybe in one of the used units
            None => {
                if path_is_relative {
                    self.find_path_in_used_units(path)
                } else {
                    None
                }
            },
        }
    }

    fn find_path_in_used_units(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        let current_path = self.scopes.current_path();

        // try it as a qualified name in a used namespaces
        // there can be multiple used units that declare the same name - if there's one
        // result, we use that, otherwise it's ambiguous
        let results: Vec<_> = current_path
            .all_used_namespaces()
            .into_iter()
            .filter_map(|use_unit| {
                let mut path_in_unit = use_unit.clone();
                path_in_unit.extend(path.iter().cloned());

                // this should only be treated as an absolute path
                self.find_path_rec(&path_in_unit, false)
            })
            .collect();

        // the last `uses` import always wins
        results.into_iter().last()
    }

    pub fn current_closure_env(&self) -> Option<&ClosureBodyEnvironment> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::ClosureBody(body_env) => return Some(body_env),
                _ => continue,
            }
        }

        None
    }

    pub fn current_function_body_env(&self) -> Option<&FunctionBodyEnvironment> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::FunctionBody(body_env) => return Some(body_env),
                _ => continue,
            }
        }

        None
    }

    // get the return type of the current function body scope. in a closure where the type hasn't
    // been inferred yet, returns the Nothing type. returns None if we're not currently in
    // a function body scope
    pub fn current_func_return_ty(&self) -> Option<&Type> {
        self.current_function_body_env()
            .map(|env| &env.result_ty)
            .or_else(|| {
                self.current_closure_env()
                    .and_then(|env| env.result_ty.as_ref().or_else(|| Some(&Type::Nothing)))
            })
    }

    // if we are in a function body context where the result type is not yet inferred, set it now.
    pub fn set_inferred_result_ty(&mut self, ty: &Type) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Environment::ClosureBody(env) = scope.env_mut() {
                if env.result_ty.is_some() {
                    return false;
                }

                env.result_ty = Some(ty.clone());
                return true;
            }
        }

        false
    }

    pub fn current_enclosing_ty(&self) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            match scope.env() {
                Environment::TypeDecl { ty, .. } => return Some(ty),
                _ => continue,
            }
        }

        None
    }

    pub fn allow_unsafe(&self) -> bool {
        for scope in self.scopes.iter().rev() {
            if let Environment::Block { allow_unsafe: true } = scope.env() {
                return true;
            }
        }

        false
    }

    fn declare(&mut self, name: Ident, decl: Decl) -> TypeResult<()> {
        let local_name_path = IdentPath::from(name.clone());

        match self.find_path(&local_name_path) {
            Some(ScopeMemberRef::Decl {
                value: Decl::Alias(aliased),
                key,
                ref parent_path,
            }) => {
                match &decl {
                    // new decl aliases the same type and is OK
                    Decl::Alias(new_aliased) if *new_aliased == *aliased => {
                        // don't need to redeclare it, it's already the same alias
                        Ok(())
                    },

                    // new name replaces an existing alias and doesn't match the old alias
                    _ => {
                        let old_ns = IdentPath::from_parts(parent_path.keys().cloned());
                        let old_ident = old_ns.child(key.clone());

                        Err(TypeError::NameError {
                            span: name.span.clone(),
                            err: NameError::AlreadyDeclared {
                                new: name,
                                existing_kind: ScopeMemberKind::Decl,
                                existing: old_ident,
                                conflict: DeclConflict::Type,
                            },
                        })
                    },
                }
            },

            Some(ScopeMemberRef::Decl {
                key: old_key,
                value: old_decl,
                parent_path,
                ..
            }) => {
                let old_ident = Path::new((*old_key).clone(), parent_path.keys().cloned());

                // re-declarations are only valid in the same scope as the original
                if parent_path.to_namespace() != self.namespace() {
                    return Err(Self::redecl_error(
                        name,
                        ScopeMemberKind::Decl,
                        old_ident,
                        DeclConflict::Name,
                    ));
                }

                if let Some(conflict) = old_decl.get_conflict(&decl) {
                    return Err(Self::redecl_error(
                        name,
                        ScopeMemberKind::Decl,
                        old_ident,
                        conflict,
                    ));
                }

                match self.get_decl_scope_mut(&name) {
                    None => Err(Self::redecl_error(
                        name,
                        ScopeMemberKind::Decl,
                        old_ident,
                        DeclConflict::Name,
                    )),

                    Some(redecl_scope) => {
                        redecl_scope.replace_member(name.clone(), ScopeMember::Decl(decl));
                        Ok(())
                    },
                }
            },

            _ => self
                .scopes
                .insert_decl(name.clone(), decl)
                .map_err(|err| TypeError::NameError {
                    err,
                    span: name.span().clone(),
                }),
        }
    }

    fn redecl_error(
        key: Ident,
        old_kind: ScopeMemberKind,
        old_ident: IdentPath,
        conflict: DeclConflict,
    ) -> TypeError {
        let err_span = key.span().clone();
        let err = NameError::AlreadyDeclared {
            new: key,
            existing_kind: old_kind,
            existing: old_ident,
            conflict,
        };

        TypeError::from_name_err(err, err_span)
    }

    pub fn declare_local_var(&mut self, name: Ident, binding: Binding) -> TypeResult<()> {
        self.declare(name, Decl::LocalVariable { binding })?;
        Ok(())
    }

    pub fn declare_global_var(&mut self,
        name: Ident,
        binding: Binding,
        visibility: Visibility
    ) -> TypeResult<()> {
        self.declare(name, Decl::GlobalVariable { binding, visibility })?;
        Ok(())
    }

    pub fn declare_iface(
        &mut self,
        iface: Rc<InterfaceDecl>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = iface.name.ident().clone();
        let iface_ty = Type::interface(iface.name.clone());

        self.declare_type(name.clone(), iface_ty, visibility, iface.forward)?;

        if !iface.forward {
            let map_unexpected = |_, _| unreachable!();
            self.define(
                name,
                DefKey::Unique,
                Def::Interface(iface.clone()),
                DefDeclMatch::always_match,
                map_unexpected,
            )?;
        }

        Ok(())
    }

    pub fn declare_variant(
        &mut self,
        variant: Rc<VariantDef>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = variant.name.ident().clone();

        let variant_ty = Type::variant(variant.name.clone());
        self.declare_type(name.clone(), variant_ty, visibility, variant.forward)?;

        if !variant.forward {
            let map_unexpected = |_, _| unreachable!();
            self.define(
                name,
                DefKey::Unique,
                Def::Variant(variant.clone()),
                DefDeclMatch::always_match,
                map_unexpected,
            )?;
        }

        Ok(())
    }

    pub fn declare_struct(
        &mut self,
        struct_def: Rc<StructDef>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = struct_def.name.ident().clone();

        let class_ty = match struct_def.kind {
            syn::StructKind::Class => Type::class(struct_def.name.clone()),
            syn::StructKind::Record => Type::record(struct_def.name.clone()),
        };

        self.declare_type(
            name.clone(),
            class_ty.clone(),
            visibility,
            struct_def.forward,
        )?;

        if !struct_def.forward {
            let map_unexpected = |_, _| unreachable!();
            self.define(
                name,
                DefKey::Unique,
                Def::Struct(struct_def.clone()),
                DefDeclMatch::always_match,
                map_unexpected,
            )?;
        }

        Ok(())
    }

    pub fn declare_enum(
        &mut self,
        enum_decl: Rc<EnumDecl>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let name = enum_decl.name.ident().clone();

        let enum_ty = Type::enumeration(enum_decl.name.full_path.clone());

        self.declare_type(name.clone(), enum_ty.clone(), visibility, false)?;

        self.define(
            name,
            DefKey::Unique,
            Def::Enum(enum_decl.clone()),
            DefDeclMatch::always_match,
            |_, _| unreachable!(),
        )?;

        for item in &enum_decl.items {
            let ord_val = item
                .value
                .as_ref()
                .expect("enum ord values must exist after typechecking");
            self.declare_global_const(
                item.ident.clone(),
                Literal::Integer(*ord_val),
                enum_ty.clone(),
                visibility,
                item.span.clone(),
            )?;
        }

        Ok(())
    }
    
    pub fn declare_set(&mut self,
        set_decl: &Rc<SetDecl>,
        visibility: Visibility
    ) -> TypeResult<()> {
        let set_type = set_decl.to_set_type(self)?;
        let decl_ty = Type::set(set_type);

        self.declare_type(set_decl.name.ident().clone(), decl_ty, visibility, false)
    }

    /// declare the type params of a function in the local scope
    pub fn declare_type_params(&mut self, names: &TypeParamList) -> TypeResult<()> {
        for param in names.items.iter() {
            let is_ty = param
                .constraint
                .as_ref()
                .map(|c| c.is_ty.clone())
                .unwrap_or(Type::Any);

            self.declare_type(
                param.name.clone(),
                Type::GenericParam(Rc::new(TypeParamType {
                    name: param.name.clone(),
                    is_ty,
                })),
                Visibility::Implementation,
                false,
            )?;
        }

        Ok(())
    }

    pub fn declare_self_ty(&mut self, ty: Type, span: Span) -> TypeResult<()> {
        let self_ident = Ident::new(SELF_TY_NAME, span);
        self.declare_type(self_ident, ty, Visibility::Implementation, false)
    }

    pub fn declare_type(
        &mut self,
        name: Ident,
        ty: Type,
        visibility: Visibility,
        forward: bool,
    ) -> TypeResult<()> {
        self.declare(
            name,
            Decl::Type {
                ty: ty.clone(),
                visibility,
                forward,
            },
        )?;

        Ok(())
    }

    pub fn is_function_declared(&self, decl: &FunctionDecl) -> bool {
        let new_sig = decl.sig();
        let func_name_path = IdentPath::from(decl.name.ident.clone());

        match self.find_function(&func_name_path) {
            Ok((_, overloads)) => overloads.iter().any(|overload| {
                **overload.sig() == new_sig
            }),

            // probably a NotFound error, but the caller is presumably about to declare the
            // function if we return false, so any other error can be handled at that point
            Err(..) => false,
        }
    }

    pub fn declare_function(
        &mut self,
        name: Ident,
        func_decl: Rc<FunctionDecl>,
        visibility: Visibility,
    ) -> TypeResult<()> {
        let current_scope = self.scopes.current_mut();
        
        match current_scope.get_decl_mut(&name) {
            Some(Decl::Function {
                overloads,
                visibility: existing_visibility,
            }) => {
                // eprintln!("found existing decl for {}", func_decl);
                
                // the visibility of the name as a whole is the visibility of its most visibile
                // member, so function calls need to specifically check their targets are visible
                *existing_visibility = Visibility::max(*existing_visibility, visibility);

                let overload_decls = overloads
                    .iter()
                    .map(|x| x.decl().clone());

                if let Some(kind) = func_decl.check_new_overload(overload_decls) {
                    return Err(TypeError::InvalidFunctionOverload {
                        ident: func_decl.name.ident.clone(),
                        prev_decls: overloads
                            .iter()
                            .map(|decl| decl.decl().name.ident.clone())
                            .collect(),
                        kind,
                    });
                }

                // eprintln!("adding new overload! {}", new_sig);
                overloads.push(DeclFunctionOverload::new(func_decl.clone(), visibility));
            },

            _ => {
                // eprintln!("{:?}: adding new decl for {}", current_scope.id(), func_decl);

                let decl = Decl::Function {
                    overloads: vec![DeclFunctionOverload::new(func_decl.clone(), visibility)],
                    visibility,
                };

                self.declare(name.clone(), decl)?;
            },
        };

        if func_decl.external_src().is_some() {
            let sig_key = DefKey::Sig(Rc::new(func_decl.sig()));
            let def = Def::External(func_decl);

            self.define(name, sig_key, def, |_| DefDeclMatch::Match, |_, _| unreachable!())?;
        }

        Ok(())
    }

    pub fn declare_alias(&mut self, name: Ident, aliased: IdentPath) -> TypeResult<()> {
        self.declare(name, Decl::Alias(aliased))
    }

    pub fn declare_local_const(
        &mut self,
        name: Ident,
        val: Literal,
        ty: Type,
        span: Span,
    ) -> TypeResult<()> {
        self.declare(
            name,
            Decl::LocalConst {
                val,
                ty,
                span,
            },
        )
    }

    pub fn declare_global_const(
        &mut self,
        name: Ident,
        val: Literal,
        ty: Type,
        visibility: Visibility,
        span: Span,
    ) -> TypeResult<()> {
        self.declare(
            name,
            Decl::GlobalConst {
                visibility,
                val,
                ty,
                span,
            },
        )
    }

    pub fn resolve_alias(&self, path: &IdentPath) -> Option<IdentPath> {
        let member = self.find_path(path)?;

        match member {
            ScopeMemberRef::Decl {
                value,
                parent_path,
                key,
            } => match value {
                Decl::Alias(aliased) => self.resolve_alias(aliased),
                _ => Some(IdentPath::new(key.clone(), parent_path.keys().cloned())),
            },

            _ => None,
        }
    }

    fn insert_method_def(&mut self, ty: Type, def: Rc<FunctionDef>) -> NameResult<()> {
        let method = &def.decl.name.ident;

        match ty {
            Type::Interface(iface_name) => {
                let iface_ty = Type::interface((*iface_name).clone());

                // check the method exists
                let generic_def = self.instantiate_iface_def(&iface_name)?;
                if generic_def.get_method(&def.decl.name.ident).is_none() {
                    return Err(NameError::MemberNotFound {
                        base: NameContainer::Type(iface_ty),
                        member: method.clone(),
                    });
                }

                self.insert_method_def_entry(iface_ty, def)
            },
            
            Type::Class(sym) => {
                self.insert_struct_method_def(def, &sym, StructKind::Class)
            }

            Type::Record(sym) => {
                self.insert_struct_method_def(def, &sym, StructKind::Record)
            },

            Type::Variant(sym) => {
                let variant_def = self.find_variant_def(&sym.full_path)?;
                let variant_ty = Type::variant((*sym).clone());

                variant_def
                    .find_methods(&method)
                    .find(|(_, m)| m.func_decl.sig() == def.decl.sig())
                    .ok_or_else(|| NameError::MemberNotFound {
                        base: NameContainer::Type(variant_ty.clone()),
                        member: method.clone(),
                    })?;

                self.insert_method_def_entry(variant_ty, def)
            },

            Type::Primitive(primitive) => {
                let ty_methods = self.get_primitive_methods(primitive);
                let primitive_ty = Type::Primitive(primitive);

                ty_methods
                    .get(method)
                    .ok_or_else(|| NameError::MemberNotFound {
                        base: NameContainer::Type(Type::Primitive(primitive)),
                        member: method.clone(),
                    })?;

                self.insert_method_def_entry(primitive_ty, def)
            },

            _ => {
                unreachable!("should only call this with types supporting methods")
            },
        }
    }
    
    fn insert_struct_method_def(&mut self,
        def: Rc<FunctionDef>,
        struct_name: &Symbol,
        struct_kind: StructKind
    ) -> NameResult<()> {
        let method = def.decl.ident();

        let struct_def = self.find_struct_def(&struct_name.full_path, struct_kind)?;
        let struct_ty = Type::from_struct_type((*struct_name).clone(), struct_def.kind);

        struct_def
            .find_methods(&method)
            .find(|(_, m)| m.func_decl.sig() == def.decl.sig())
            .ok_or_else(|| NameError::MemberNotFound {
                base: NameContainer::Type(struct_ty.clone()),
                member: method.clone(),
            })?;

        self.insert_method_def_entry(struct_ty, def)
    }

    fn insert_method_def_entry(&mut self, ty: Type, def: Rc<FunctionDef>) -> NameResult<()> {
        let methods = self
            .method_defs
            .entry(ty.clone())
            .or_insert_with(MethodCollection::new);

        let key = MethodKey {
            name: def.decl.ident().clone(),
            sig: Rc::new(def.decl.sig()),
        };

        match methods.methods.entry(key) {
            Entry::Vacant(vacant) => {
                vacant.insert(Some(def));
                Ok(())
            },

            Entry::Occupied(mut occupied) => match occupied.get_mut() {
                Some(existing) => Err(NameError::AlreadyDefined {
                    ident: IdentPath::from(def.decl.name.ident.clone()),
                    existing: existing.decl.span().clone(),
                }),

                empty => {
                    *empty = Some(def);
                    Ok(())
                },
            },
        }
    }

    pub fn define_method(&mut self, ty: Type, method_def: Rc<FunctionDef>) -> TypeResult<()> {
        let span = method_def.decl.span().clone();
        self.insert_method_def(ty, method_def)
            .map_err(|err| TypeError::from_name_err(err, span))?;

        Ok(())
    }

    pub fn find_method(&self, ty: &Type, method: &Ident, sig: &Rc<FunctionSig>) -> Option<&FunctionDef> {
        let method_defs = self.method_defs.get(ty)?;
        
        let key = MethodKey {
            name: method.clone(),
            sig: sig.clone(),
        };
        
        // if !method_defs.methods.get(&key).is_some() {
        //     eprintln!("missing key for {}.{}: {}", ty, key.name, key.sig);
        //     eprintln!("keys are:");
        //     for key in method_defs.methods.keys() {
        //         eprintln!(" - {}: {}", key.name, key.sig);
        //     }
        // }

        let method = method_defs.methods.get(&key)?.as_ref()?;

        Some(&method)
    }

    pub fn namespace(&self) -> IdentPath {
        IdentPath::from_parts(self.scopes.current_path().keys().cloned())
    }
    
    pub fn is_current_namespace(&self, path: &IdentPath) -> bool {
        for (i, key) in self.scopes.current_path().keys().enumerate() {
            match path.as_slice().get(i) {
                Some(part) if part == key => continue,
                _ => return false,
            }
        }
        
        true
    }

    pub fn is_current_namespace_child(&self, path: &IdentPath) -> bool {
        for (i, key) in self.scopes.current_path().keys().enumerate() {
            if i == path.len() - 1 {
                return false;
            }
            
            match path.as_slice().get(i) {
                Some(part) if part == key => continue,
                _ => return false,
            }
        }

        true
    }

    pub fn qualify_name(&self, name: Ident) -> IdentPath {
        self.namespace().child(name)
    }

    fn define<DeclPred, MapUnexpected>(
        &mut self,
        name: Ident,
        def_key: DefKey,
        def: Def,
        decl_predicate: DeclPred,
        map_unexpected: MapUnexpected,
    ) -> TypeResult<()>
    where
        DeclPred: Fn(&Decl) -> DefDeclMatch,
        MapUnexpected: Fn(IdentPath, Named) -> NameError,
    {
        // must be previously declared
        let decl_ref = match self.scopes.current_path().find(&name) {
            None => {
                return Err(TypeError::NameError {
                    err: NameError::not_found(name),
                    span: def.ident().span().clone(),
                });
            },

            Some(decl) => decl,
        };

        let full_name = match decl_ref {
            ScopeMemberRef::Decl {  value, parent_path, key } => {
                // a decl already exists, find the existing def
                let full_name = parent_path.to_namespace().child(name);

                match decl_predicate(value) {
                    DefDeclMatch::Match => {
                        // if there's a matching existing decl, and a def already exists for this
                        // key, throw an error unless it's a valid redecl
                        if let Some(existing) = self.find_def(&full_name, &def_key) {
                            // allow the redeclaration of certain builtin types in the System unit,
                            // as long as the new definition matches the builtin one exactly
                            if !value.is_valid_builtin_redecl() || *existing != def {
                                // dbg!(existing);
                                // dbg!(&def);

                                let err = NameError::AlreadyDefined {
                                    ident: full_name,
                                    existing: existing.ident().span().clone(),
                                };

                                return Err(TypeError::NameError {
                                    err,
                                    span: def.ident().span().clone(),
                                });
                            }
                        }
                    },

                    DefDeclMatch::Mismatch => {
                        return Err(TypeError::NameError {
                            err: NameError::DefDeclMismatch {
                                decl: key.span().clone(),
                                def: def.ident().span().clone(),
                                path: full_name,
                            },
                            span: def.ident().span.clone(),
                        });
                    },

                    DefDeclMatch::WrongKind => {
                        let unexpected = Named::Decl(value.clone());
                        let err = map_unexpected(full_name, unexpected);
                        return Err(TypeError::NameError {
                            err,
                            span: def.span().clone(),
                        });
                    },
                }
                
                full_name
            },

            ScopeMemberRef::Scope { path } => {
                let namespace = path.to_namespace();

                let path = IdentPath::from_parts(path.keys().cloned());
                let err = map_unexpected(namespace.child(name), Named::Namespace(path));

                return Err(TypeError::NameError {
                    err,
                    span: def.span().clone(),
                });
            },
        };

        self.defs
            .entry(full_name)
            .or_insert_with(HashMap::new)
            .insert(def_key, def);

        Ok(())
    }

    pub fn define_function(&mut self, name: Ident, def: Rc<FunctionDef>) -> TypeResult<()> {
        let sig = Rc::new(def.decl.sig());
        let kind = def.decl.kind;

        // defining a function - only the sig needs to match, the visibility of the definition doesn't matter
        // since the implementation of an interface func can be in the implementation section, but the implementation
        // of an implementation func can't be in the interface section, since it comes last
        let is_func_decl = |decl: &Decl| match decl {
            Decl::Function { overloads, .. } => {
                let decl_exists = overloads.iter().any(|overload| {
                    let existing_sig = overload.sig().as_ref();
                    let existing_kind = overload.decl().kind;
                    existing_kind == kind && *existing_sig == *sig
                });

                if !decl_exists {
                    DefDeclMatch::Mismatch
                } else {
                    DefDeclMatch::Match
                }
            },
            _ => DefDeclMatch::WrongKind,
        };

        let expected_func_err = |ident, unexpected| NameError::Unexpected {
            ident,
            expected: ExpectedKind::Function,
            actual: unexpected,
        };

        let sig_key = DefKey::Sig(sig.clone()); 
        self.define(name, sig_key, Def::Function(def), is_func_decl, expected_func_err)
    }

    pub fn find_type(&self, name: &IdentPath) -> NameResult<(IdentPath, &Type)> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
                value: Decl::Type { ty, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((parent_path, ty))
            },

            Some(ScopeMemberRef::Decl { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                expected: ExpectedKind::AnyType,
                actual: other.clone().into(),
            }),

            Some(ScopeMemberRef::Scope { path }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: Named::Namespace(IdentPath::from_parts(path.keys().cloned())),
                expected: ExpectedKind::AnyType,
            }),

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn defined_types(&self) -> Vec<Type> {
        self.defs
            .iter()
            .flat_map(|(_, def_map)| def_map.iter())
            .filter_map(|(def_key, def)| {
                match def_key {
                    DefKey::Unique => Some(def),
                    DefKey::Sig(..) => None,
                }
            })
            .filter_map(|def| match def {
                Def::Struct(struct_def) => {
                    Some(Type::from_struct_type(struct_def.name.clone(), struct_def.kind))
                },

                Def::Variant(variant_def) => {
                    Some(Type::variant(variant_def.name.clone()))
                },

                Def::Interface(iface_def) => {
                    Some(Type::interface(iface_def.name.clone()))
                },

                Def::Set(set_def) => {
                    let set_type=  set_def
                        .to_set_type(self)
                        .unwrap_or_else(|err| {
                            panic!("defined set type {} was invalid: {}", set_def.name, err)
                        });

                    Some(Type::set(set_type))
                },

                Def::Enum(enum_def) => {
                    Some(Type::enumeration(enum_def.name.full_path.clone()))
                },
                
                _ => None,
            })
            .collect()
    }

    pub fn find_def(&self, name: &IdentPath, def_key: &DefKey) -> Option<&Def> {
        self.defs.get(name).and_then(|overloads| overloads.get(def_key))
    }

    pub fn find_type_def(&self, name: &IdentPath) -> Option<&Def> {
        self.find_def(name, &DefKey::Unique)
    }

    pub fn find_func_def(&self, name: &IdentPath, sig: Rc<FunctionSig>) -> Option<&Def> {
        self.find_def(name, &DefKey::Sig(sig))
    }

    pub fn find_struct_def(&self, name: &IdentPath, kind: StructKind) -> NameResult<&Rc<StructDef>> {
        match self.find_type_def(name) {
            Some(Def::Struct(struct_def)) if struct_def.kind == kind => {
                Ok(struct_def)
            }

            Some(..) => {
                let expected = match kind {
                    StructKind::Class => ExpectedKind::Class,
                    StructKind::Record => ExpectedKind::Record,
                };

                Err(self.unexpected_err_for_existing_def(name, expected))
            },

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn instantiate_struct_def(&self, name: &Symbol, kind: StructKind) -> NameResult<Rc<StructDef>> {
        name.expect_not_unspecialized()?;

        let generic_def = self.find_struct_def(&name.full_path, kind)?;

        let specialized_def = match &name.type_args {
            Some(type_args) => specialize_struct_def(generic_def, type_args, self)?,
            None => generic_def.clone(),
        };

        Ok(specialized_def)
    }

    pub fn find_variant_def(&self, name: &IdentPath) -> NameResult<&Rc<VariantDef>> {
        match self.find_type_def(name) {
            Some(Def::Variant(variant_def)) => Ok(variant_def),

            Some(..) => {
                Err(self.unexpected_err_for_existing_def(name, ExpectedKind::Variant))
            },

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn instantiate_variant_def(&self, name: &Symbol) -> NameResult<Rc<VariantDef>> {
        name.expect_not_unspecialized()?;

        let base_def = self.find_variant_def(&name.full_path)?;
        
        let Some(type_args) = &name.type_args else {
            assert!(name.type_args.is_none(), "name must not be specialized for non-generic variant type");
            
            return Ok(base_def.clone());
        };

        let instance_def = specialize_variant_def(base_def.as_ref(), type_args, self)?;
        Ok(Rc::new(instance_def))
    }

    pub fn find_iface(&self, name: &IdentPath) -> NameResult<IdentPath> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
                value:
                    Decl::Type {
                        ty: Type::Interface(..),
                        ..
                    },
                key,
                ref parent_path,
                ..
            }) => {
                let parent_path = Path::new(key.clone(), parent_path.keys().cloned());

                Ok(parent_path)
            },

            Some(ScopeMemberRef::Decl { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: other.clone().into(),
                expected: ExpectedKind::Interface,
            }),

            Some(ScopeMemberRef::Scope { path }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: Named::Namespace(path.to_namespace()),
                expected: ExpectedKind::Interface,
            }),

            None => Err(NameError::not_found(name.clone())),
        }
    }

    pub fn find_iface_def(&self, name: &IdentPath) -> NameResult<&Rc<InterfaceDecl>> {
        match self.find_type_def(name) {
            Some(Def::Interface(iface_def)) => Ok(iface_def),

            Some(..) => {
                Err(self.unexpected_err_for_existing_def(name, ExpectedKind::Interface))
            },

            None => Err(NameError::not_found(name.clone())),
        }
    }
    
    pub fn instantiate_iface_def(&self, name: &Symbol) -> NameResult<Rc<InterfaceDecl>> {
        name.expect_not_unspecialized()?;

        let generic_def = self.find_iface_def(&name.full_path)?;
        
        let Some(type_args) = &name.type_args else {
            return Ok(generic_def.clone());
        };
        
        let specialized_def = specialize_iface_def(generic_def, type_args, self)?;
        Ok(specialized_def)
    }
    
    pub fn find_set(&self, name: &IdentPath) -> NameResult<&Rc<SetDecl>> {
        match self.find_type_def(name) {
            Some(Def::Set(set_decl)) => {
                Ok(set_decl)
            },

            Some(..) => {
                Err(self.unexpected_err_for_existing_def(name, ExpectedKind::SetDecl))
            }
            
            None => {
                Err(NameError::not_found(name.clone()))
            }
        }
    }
    
    fn unexpected_err_for_existing_def(
        &self,
        name: &IdentPath,
        expected: ExpectedKind
    ) -> NameError {
        let decl = self
            .find_path(&name)
            .expect("found def so decl must exist")
            .as_value()
            .expect("if def exists it must be a value not a namespace")
            .clone();

        let unexpected = Named::Decl(decl);

        NameError::Unexpected {
            ident: name.clone(),
            actual: unexpected,
            expected,
        }
    }

    pub fn is_implementation(&self, self_ty: &Type, iface_ty: &Type) -> NameResult<bool> {
        if self_ty == iface_ty {
            return Ok(true);
        }

        match self_ty {
            Type::GenericParam(param_ty) => match &param_ty.is_ty {
                Type::Any => Ok(false),

                as_iface => Ok(as_iface == iface_ty),
            },

            Type::Primitive(..) => Ok(self.primitive_implements.contains(iface_ty)),

            Type::Record(name) | Type::Class(name) => {
                let struct_kind = self_ty.struct_kind().unwrap();
                let def = self.instantiate_struct_def(name, struct_kind)?;
                
                for implements_ty in &def.implements {
                    if self.is_implementation(implements_ty, iface_ty)? {
                        return Ok(true);
                    }
                }
                
                Ok(false)
            },
            
            Type::Interface(name) => {
                let def = self.instantiate_iface_def(name)?;
                
                for super_ty in &def.supers {
                    if self.is_implementation(super_ty, iface_ty)? {
                        return Ok(true);
                    }
                }

                Ok(false)
            }

            _ => Ok(false),
        }
    }

    pub fn is_implementation_at(
        &self,
        self_ty: &Type,
        iface_ty: &Type,
        at: &Span,
    ) -> TypeResult<bool> {
        match self.is_implementation(self_ty, iface_ty) {
            Ok(is_impl) => Ok(is_impl),
            Err(err) => Err(TypeError::from_name_err(err, at.clone())),
        }
    }

    pub fn find_function(&self, name: &IdentPath) -> NameResult<(IdentPath, &[DeclFunctionOverload])> {
        match self.find_path(name) {
            Some(ScopeMemberRef::Decl {
                value: Decl::Function { overloads, .. },
                key,
                ref parent_path,
                ..
            }) => {
                let func_path = Path::new(key.clone(), parent_path.keys().cloned());
                Ok((func_path, overloads))
            },

            Some(ScopeMemberRef::Decl { value: other, .. }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: other.clone().into(),
                expected: ExpectedKind::Function,
            }),

            Some(ScopeMemberRef::Scope { path }) => Err(NameError::Unexpected {
                ident: name.clone(),
                actual: Named::Namespace(path.to_namespace()),
                expected: ExpectedKind::Function,
            }),

            None => Err(NameError::NotFound {
                ident: name.clone(),
            }),
        }
    }

    /// Get the methods implemented by this primitive type
    pub fn get_primitive_methods(&self, primitive: Primitive) -> &LinkedHashMap<Ident, MethodDecl> {
        &self.primitive_methods[&primitive]
    }

    /// Get the interface types implemented by this primitive type
    pub fn get_primitive_impls(&self, _primitive: Primitive) -> &[Type] {
        &self.primitive_implements
    }

    pub fn find_instance_member<'ty, 'ctx: 'ty>(
        &'ctx self,
        of_ty: &'ty Type,
        member: &Ident,
    ) -> NameResult<InstanceMember> {
        let data_member = of_ty.find_data_member(member, self)?;

        let methods = ufcs::find_instance_methods_of(of_ty, self)?;
        let matching_methods: Vec<_> = methods.iter().filter(|m| *m.ident() == *member).collect();

        match (data_member, matching_methods.len()) {
            (Some(field), 0) => Ok(InstanceMember::Field {
                ty: field.ty,
                access: field.access,
            }),

            // unambiguous method
            (None, 1) => match matching_methods.last().unwrap() {
                ufcs::InstanceMethod::Method {
                    iface_ty,
                    self_ty,
                    method,
                    ..
                } => Ok(InstanceMember::Method {
                    iface_ty: iface_ty.clone(),
                    self_ty: self_ty.clone(),
                    method: method.clone(),
                }),

                ufcs::InstanceMethod::FreeFunction { func_name, decl, visibility } => {
                    Ok(InstanceMember::UFCSCall {
                        func_name: func_name.clone(),
                        decl: decl.clone(),
                        visibility: *visibility,
                    })
                },
            },

            // no data member, no methods
            (None, 0) => Err(NameError::MemberNotFound {
                member: member.clone(),
                base: NameContainer::Type(of_ty.clone()),
            }),

            // no data member, multiple methods - we can use overloading to determine which
            (None, _) => {
                let candidates: Vec<_> = matching_methods
                    .iter()
                    .map(|m| OverloadCandidate::from_instance_method((**m).clone()))
                    .collect();

                Ok(InstanceMember::Overloaded { candidates })
            },

            // there's a data member AND 1+ methods
            (Some(data_member), _) => Err(NameError::Ambiguous {
                ident: member.clone(),
                options: ambig_paths(
                    ambig_matching_methods(&matching_methods)
                        .into_iter()
                        .chain(vec![(of_ty.clone(), data_member.ident)]),
                ),
            }),
        }
    }

    pub fn is_unsized_ty(&self, ty: &Type) -> NameResult<bool> {
        match ty {
            Type::Nothing | Type::MethodSelf => Ok(true),

            Type::Record(class) => {
                match self.find_struct_def(&class.full_path, StructKind::Record) {
                    Ok(def) => Ok(def.forward),
                    Err(NameError::NotFound { .. }) => Ok(true),
                    Err(err) => Err(err),
                }
            },

            Type::Variant(variant) => match self.find_variant_def(&variant.full_path) {
                Ok(def) => Ok(def.forward),
                Err(NameError::NotFound { .. }) => Ok(true),
                Err(err) => Err(err.into()),
            },

            Type::Array(array_ty) => self.is_unsized_ty(&array_ty.element_ty),

            Type::Weak(weak_ty) => self.is_unsized_ty(weak_ty),

            Type::Any
            | Type::Class(..)
            | Type::GenericParam(_)
            | Type::Interface(..)
            | Type::Pointer(..)
            | Type::Primitive(..)
            | Type::Nil
            | Type::Function(..)
            | Type::DynArray { .. }
            | Type::Enum(..)
            | Type::Set(..) => Ok(false),
        }
    }

    // for a given base type, find the member (currently always a method) that matches a name.
    // if the method or type is generic, we might infer a specialized type for the base type
    // depending on the return type, e.g. in the following case:
    //   `var x: Integer := MyType.Value`
    // where the method found is `MyType[T].Value: T`, we can infer from the return type that
    // in this call, the type is `MyType[Integer]`
    pub fn find_type_member(
        &self,
        ty: &Type,
        member_ident: &Ident,
        expect_return_ty: &Type,
        span: &Span,
        ctx: &Context,
    ) -> TypeResult<TypeMember> {
        let result = match ty {
            Type::Interface(iface_sym) => {
                let iface_def = self
                    .instantiate_iface_def(&iface_sym)
                    .map_err(|e| TypeError::from_name_err(e, span.clone()))?;
                
                let methods: Vec<_> = iface_def.methods
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| m.decl.ident() == member_ident)
                    .collect();

                if !methods.is_empty() {
                    let mut method_group = Vec::new();

                    for (method_index, method) in methods {
                        let method_sig = method.decl.sig();

                        let spec_iface_sym =
                            specialize_by_return_ty(
                                iface_sym,
                                &method_sig,
                                expect_return_ty,
                                span,
                                ctx,
                            )
                            .map_err(|e| TypeError::from_generic_err(e, span.clone()))?
                            .into_owned();

                        let spec_def = ctx
                            .instantiate_iface_def(&spec_iface_sym)
                            .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

                        let spec_method = spec_def.methods[method_index].clone();

                        method_group.push(MethodGroupMember {
                            method: MethodDecl {
                                func_decl: spec_method.decl,
                                access: IFACE_METHOD_ACCESS,
                            },
                            index: method_index,
                            iface_ty: Type::interface(spec_iface_sym),
                        })
                    }

                    Some(TypeMember::from_method_members(method_group))
                } else {
                    None
                }
            }

            Type::Class(struct_name) | Type::Record(struct_name) => {
                let struct_kind = ty.struct_kind().unwrap();

                // start by looking for methods in the unspecialized struct def, if it's a 
                // method call we might need to infer the struct's own type args from the call
                let struct_def = self
                    .find_struct_def(&struct_name.full_path, struct_kind)
                    .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

                let methods: Vec<_> = struct_def
                    .methods()
                    .enumerate()
                    .filter(|(_, method)| {
                        method.func_decl.ident() == member_ident
                    })
                    .collect();

                if !methods.is_empty() {
                    let mut method_group = Vec::new();

                    for (method_index, method) in methods {
                        let method_sig = method.func_decl.sig();

                        let spec_struct_name = 
                            specialize_by_return_ty(
                                struct_name,
                                &method_sig,
                                expect_return_ty,
                                span,
                                ctx,
                            )
                            .map_err(|e| TypeError::from_generic_err(e, span.clone()))?
                            .into_owned();

                        let spec_def = ctx
                            .instantiate_struct_def(&spec_struct_name, struct_kind)
                            .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

                        let spec_method = spec_def.methods[method_index].clone();

                        method_group.push(MethodGroupMember {
                            method: spec_method,
                            index: method_index,
                            iface_ty: Type::from_struct_type(spec_struct_name, struct_def.kind),
                        })
                    }
                    
                    Some(TypeMember::from_method_members(method_group))
                } else {
                    None
                }
            }

            Type::Variant(variant_name) => {
                let variant_def = self
                    .find_variant_def(&variant_name.full_path)
                    .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

                let methods: Vec<_> = variant_def
                    .methods
                    .iter()
                    .enumerate()
                    .filter(|(_, method)| {
                        method.func_decl.ident() == member_ident
                    })
                    .collect();

                if !methods.is_empty() {
                    let mut method_group = Vec::new();

                    for (method_index, generic_method) in methods {
                        let method_sig = generic_method.func_decl.sig();

                        let spec_variant_name =
                            specialize_by_return_ty(
                                variant_name,
                                &method_sig,
                                expect_return_ty,
                                span,
                                ctx,
                            )
                            .map_err(|e| TypeError::from_generic_err(e, span.clone()))?
                            .into_owned();

                        let spec_def = ctx
                            .instantiate_variant_def(&spec_variant_name)
                            .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

                        let spec_method = spec_def.methods[method_index].clone();

                        method_group.push(MethodGroupMember {
                            method: spec_method,
                            index: method_index,
                            iface_ty: Type::variant(spec_variant_name),
                        })
                    }

                    Some(TypeMember::from_method_members(method_group))
                } else {
                    None
                }
            },

            _ => None,
        };

        result.ok_or_else(|| {
            TypeError::from_name_err(
                NameError::MemberNotFound {
                    member: member_ident.clone(),
                    base: NameContainer::Type(ty.clone()),
                },
                span.clone(),
            )
        })
    }

    pub fn undefined_syms(&self) -> Vec<(IdentPath, Span)> {
        let mut syms = Vec::new();

        let current_path = self.scopes.current_path();
        let current_scopes = current_path.as_slice();

        for i in (0..current_scopes.len()).rev() {
            let scope = current_scopes[i];

            let current_scope_ns =
                IdentPath::from_parts(current_scopes[0..=i].iter().flat_map(|s| s.key()).cloned());

            // only functions and methods can possibly be undefined
            for (ident, decl) in scope.members() {
                match decl {
                    ScopeMember::Decl(Decl::Type { ty, forward, .. }) => {
                        if *forward {
                            // all forward-declared types must be fully defined within the unit
                            let missing = match ty.full_path() {
                                Some(name) => {
                                    let name_span = name.span().clone();
                                    (name.into_owned(), name_span)
                                },
                                None => {
                                    let name = current_scope_ns.clone().child(ident.clone());
                                    (name, ident.span.clone())
                                },
                            };
                            syms.push(missing)
                        } else {
                            // all types except interfaces must define any methods they declare
                            syms.append(&mut self.undefined_ty_members(ty));
                        }
                    },

                    ScopeMember::Decl(Decl::Function { overloads, .. }) => {
                        let decl_path = current_scope_ns.clone().child(ident.clone());

                        for overload in overloads {
                            if self.find_func_def(&decl_path, overload.sig().clone()).is_none() {
                                // eprintln!("undefined: {}", decl_path);
                                let decl_span = overload.decl().span().clone();
                                syms.push((decl_path.clone(), decl_span));
                            }
                        }
                    },

                    _ => {},
                }
            }
        }

        syms
    }

    fn undefined_ty_members(self: &Context, ty: &Type) -> Vec<(IdentPath, Span)> {
        if ty.as_iface().is_some() {
            return Vec::new();
        }

        let ty_name = match ty.full_path() {
            Some(path) => path,
            None => return Vec::new(),
        };

        let ty_method_defs = self.method_defs.get(&ty);

        let ty_methods = ty
            .methods(self)
            .expect("illegal state: undefined_ty_members failed to get methods from type");

        let mut missing = Vec::new();

        for method in ty_methods {
            let def_key = MethodKey {
                name: method.func_decl.ident().clone(),
                sig: Rc::new(method.func_decl.sig()),
            };
            
            // don't need to check the sigs again, they wouldn't be added
            // if they didn't match
            let has_def = ty_method_defs
                .and_then(|method_defs| {
                    method_defs
                        .methods
                        .get(&def_key)
                        .map(|def| def.is_some())
                })
                .unwrap_or(false);

            if !has_def {
                eprintln!("no def for {}: {}\ndefs are: {}", def_key.name, def_key.sig, ty_method_defs
                    .cloned()
                    .unwrap_or_else(|| MethodCollection::new())
                    .methods
                    .keys()
                    .filter(|k| k.name == *method.func_decl.ident())
                    .map(|k| format!("{}: {}", k.name, k.sig))
                    .collect::<Vec<_>>()
                    .join(",\n\t"));
                
                let decl_name = ty_name
                    .clone()
                    .into_owned()
                .child(method.func_decl.ident().clone());

                let decl_span = method.func_decl.span.clone();
                missing.push((decl_name, decl_span));
            }
        }

        missing
    }

    /// Mark a local decl as initialized.
    /// No effect if the decl exists and is already initialized.
    /// Panics if the decl doesn't exist or isn't a kind of decl which can be initialized.
    pub fn initialize(&mut self, local_id: &Ident) {
        for scope in self.scopes.iter_mut().rev() {
            let Some(member) = scope.get_decl_mut(local_id) else {
                continue;
            };

            match member {
                Decl::GlobalVariable { .. } => {
                    // ignore attempts to initialize global vars, they behave like mutable values
                    // but are assumed to be always initialized
                    return;
                }

                Decl::LocalVariable { binding: Binding { kind, .. } } => {
                    if *kind == ValueKind::Uninitialized || *kind == ValueKind::Mutable {
                        *kind = ValueKind::Mutable;

                        return;
                    } else {
                        panic!("{} does not refer to a mutable binding", local_id);
                    }
                }

                _ => continue,
            }
        }

        panic!(
            "called initialize() on an id which isn't an initializable binding in this scope: {}",
            local_id
        )
    }

    pub fn root_scope(&self) -> &Scope {
        self.scopes.iter().nth(0).unwrap()
    }

    pub fn get_closure_scope(&self) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            if let Environment::ClosureBody { .. } = scope.env() {
                return Some(scope);
            }
        }

        None
    }

    pub fn add_closure_capture(&mut self, name: &Ident, ty: &Type) {
        for scope in self.scopes.iter_mut().rev() {
            if let Environment::ClosureBody(body) = scope.env_mut() {
                if let Some(old_ty) = body.captures.insert(name.clone(), ty.clone()) {
                    if old_ty != *ty {
                        panic!("closure capture did not match previous type: {} (declared as {}, was previously {})", name, ty, old_ty)
                    }
                }
                return;
            }
        }

        panic!("called add_closure_capture without a closure scope active");
    }

    pub fn get_decl_scope(&self, id: &Ident) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            if scope.get_decl(id).is_some() {
                return Some(scope);
            }
        }

        None
    }

    pub fn get_decl_scope_mut(&mut self, id: &Ident) -> Option<&mut Scope> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.get_decl_mut(id).is_some() {
                return Some(scope);
            }
        }

        None
    }

    pub fn consolidate_branches(&mut self, branch_contexts: &[Self]) {
        let scope = self.scopes.current_path();

        let uninit_names: Vec<_> = scope
            .top()
            .members()
            .filter_map(|(ident, decl)| match decl {
                ScopeMember::Decl(Decl::LocalVariable { binding: Binding {
                    kind: ValueKind::Uninitialized,
                    ..
                }}) => Some(ident),
                _ => None,
            })
            .collect();
        let this_depth = scope.as_slice().len();

        // names initialized in all branches
        let mut all_init = Vec::new();
        for uninit_name in uninit_names {
            let is_init_in_all = branch_contexts
                .iter()
                .all(|ctx| match ctx.find_name(&uninit_name).unwrap() {
                    ScopeMemberRef::Decl {
                        value, parent_path, ..
                    } => match value {
                        Decl::LocalVariable { binding, .. } => {
                            parent_path.as_slice().len() == this_depth
                                && binding.kind == ValueKind::Mutable
                        },

                        _ => false,
                    },

                    ScopeMemberRef::Scope { .. } => false,
                });

            if is_init_in_all {
                all_init.push(uninit_name.clone());
            }
        }

        for name in all_init {
            self.initialize(&name);
        }
    }

    pub fn is_visible(&self, name: &IdentPath) -> bool {
        self.scopes.is_visible(name)
    }

    pub fn get_access(&self, name: &IdentPath) -> Access {
        self.scopes.get_access(name)
    }
}

fn ambig_paths<'a>(options: impl IntoIterator<Item = (Type, Ident)>) -> Vec<IdentPath> {
    options
        .into_iter()
        .map(|(of_ty, ident)| match of_ty.full_path() {
            Some(base) => base.into_owned().child(ident.clone()),
            None => Path::new(ident.clone(), Vec::new()),
        })
        .collect()
}

fn ambig_matching_methods(methods: &[&ufcs::InstanceMethod]) -> Vec<(Type, Ident)> {
    methods
        .iter()
        .map(|im| match im {
            ufcs::InstanceMethod::Method { self_ty, method, .. } => {
                (self_ty.clone(), method.func_decl.name.ident.clone())
            }

            ufcs::InstanceMethod::FreeFunction { decl, func_name, .. } => {
                let of_ty = decl.params.first().unwrap().ty.clone();
                (of_ty.clone(), func_name.ident().clone())
            },
        })
        .collect()
}
