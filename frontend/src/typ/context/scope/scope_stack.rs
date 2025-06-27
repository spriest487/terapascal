use crate::ast::Access;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::typ::Decl;
use crate::typ::DeclConflict;
use crate::typ::NameError;
use crate::typ::NameResult;
use crate::typ::Scope;
use crate::typ::ScopeMember;
use crate::typ::ScopeMemberRef;
use crate::typ::ScopePathRef;
use crate::typ::ScopePathRefMut;
use std::fmt;
use std::mem;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    pub fn new(root: Scope) -> Self {
        Self { scopes: vec![root] }
    }

    pub fn push_scope(&mut self, ns: Scope) {
        self.scopes.push(ns)
    }

    pub fn pop_scope(&mut self) -> Scope {
        if self.scopes.len() == 1 {
            panic!("can't pop the root scope");
        }

        let popped = self.scopes.pop().expect("pop called with no active scopes");

        if let Some(popped_key) = popped.key().cloned() {
            let current = self.scopes.last_mut().unwrap();
            if current
                .try_add_member(&popped_key, ScopeMember::Scope(popped.clone()))
                .is_err()
            {
                unreachable!(
                    "should never be possible to declare something with same key as current scope"
                );
            }
        }

        popped
    }

    pub fn insert_decl(&mut self, member_key: impl Into<Ident>, decl: Decl) -> NameResult<()> {
        let member_key = member_key.into();

        // eprintln!("{}.{}: {} ", self.current_path().to_namespace(), member_key, decl.to_string());

        let top = self.current_mut();

        let new_key = member_key.clone();
        let new_member = ScopeMember::Decl(decl);

        if let Err((existing_key, existing_member)) = top.try_add_member(&new_key, new_member) {
            let existing_path = self
                .current_path()
                .to_namespace()
                .child(existing_key.clone());

            return Err(NameError::AlreadyDeclared {
                new: new_key,
                existing: existing_path,
                existing_kind: existing_member.kind(),
                conflict: DeclConflict::Name,
            });
        }

        Ok(())
    }

    // todo: different error codes for "not defined at all" vs "defined but not in the current scope"
    pub fn replace_decl(&mut self, key: impl Into<Ident>, decl: Decl) -> NameResult<()> {
        let member_key = key.into();
        let top = self.current_mut();

        match top.get_member(&member_key) {
            Some(_) => {
                top.replace_member(member_key, ScopeMember::Decl(decl));
                Ok(())
            },

            None => Err(NameError::NotFound {
                ident: IdentPath::from(member_key),
            }),
        }
    }

    pub fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = &Scope> + DoubleEndedIterator<Item = &Scope> {
        self.scopes.iter()
    }

    pub fn current_path(&self) -> ScopePathRef {
        ScopePathRef {
            scopes: self.scopes.iter().collect(),
        }
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Scope> + DoubleEndedIterator<Item = &mut Scope> {
        self.scopes.iter_mut()
    }

    pub fn current_path_mut(&mut self) -> ScopePathRefMut {
        ScopePathRefMut {
            namespaces: self.scopes.iter_mut().collect(),
        }
    }

    pub fn current(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn resolve_path(&self, path: &IdentPath) -> Option<ScopeMemberRef> {
        let current_path = self.current_path();

        let mut next_path = current_path.clone();
        for (i, part) in path.iter().enumerate() {
            // special case because the namespace stack may not contain namespaces that are under
            // construction during this call e.g. if we are looking for decl A.B.C but A.B is the
            // current namespace, A won't contain B - it isn't stored there until it gets popped
            if next_path.is_parent_of(&current_path) {
                // if the next named part in the current path is the part we're looking for, the
                // path we're looking for is the current path
                let next_named_part = current_path
                    .scopes
                    .iter()
                    .skip(next_path.scopes.len())
                    .find_map(|s| s.key());

                if next_named_part == Some(part) {
                    next_path = current_path.clone();
                    continue;
                }
            }

            match next_path.find(part)? {
                ScopeMemberRef::Scope { path } => {
                    // found a namespace that matches this part, look for the next part inside
                    // that namespace
                    next_path = path;
                },

                ScopeMemberRef::Decl {
                    key,
                    value,
                    parent_path,
                } => {
                    let is_last = i == path.len() - 1;

                    return if is_last {
                        Some(ScopeMemberRef::Decl {
                            parent_path,
                            key,
                            value,
                        })
                    } else {
                        None
                    };
                },
            }
        }

        Some(ScopeMemberRef::Scope { path: next_path })
    }

    pub fn visit_members<Predicate, Visitor>(&self, predicate: Predicate, mut visitor: Visitor)
    where
        Predicate: Fn(&IdentPath, &ScopeMember) -> bool,
        Visitor: FnMut(&IdentPath, &Decl),
    {
        // reused IdentPath instance so we don't need to reallocate this for every single entry
        // we can't make an empty IdentPath so we create this in the first iteration for each scope
        let mut member_path_parts = Vec::new();

        for ns_index in (0..self.scopes.len()).rev() {
            let ns_keys = self.scopes[0..=ns_index]
                .iter()
                .filter_map(|ns| ns.key())
                .cloned();
            member_path_parts.clear();
            member_path_parts.extend(ns_keys);

            for (key, member) in self.scopes[ns_index].members() {
                member_path_parts.push(key.clone());

                let mut member_path = IdentPath::from_vec(member_path_parts);
                visit_member(&mut member_path, member, &predicate, &mut visitor);
                member_path_parts = member_path.into_vec();

                member_path_parts.pop();
            }
        }
    }

    pub fn visit_visible<Visitor>(&self, visitor: Visitor)
    where
        Visitor: FnMut(&IdentPath, &Decl),
    {
        self.visit_members(
            |member_path, _member| self.is_visible(&member_path),
            visitor,
        );
    }

    pub fn get_access(&self, name: &IdentPath) -> Access {
        let current_path = self.current_path();

        match self.resolve_path(name) {
            Some(ScopeMemberRef::Decl { parent_path, .. }) => {
                let current_ns = current_path.to_namespace();

                let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());
                if current_ns == decl_unit_ns || current_ns.is_parent_of(&decl_unit_ns) {
                    Access::Private
                } else {
                    Access::Public
                }
            },

            _ => Access::Public,
        }
    }

    pub fn is_visible(&self, name: &IdentPath) -> bool {
        let current_path = self.current_path();

        match self.resolve_path(name) {
            Some(ScopeMemberRef::Decl {
                parent_path, value, ..
            }) => {
                let current_ns = current_path.to_namespace();

                match value.visibility() {
                    Some(Visibility::Interface) => true,

                    Some(Visibility::Implementation) => {
                        let decl_unit_ns = IdentPath::from_parts(parent_path.keys().cloned());
                        current_ns == decl_unit_ns || current_ns.is_parent_of(&decl_unit_ns)
                    },

                    None => false,
                }
            },

            Some(ScopeMemberRef::Scope { .. }) => {
                let current_uses = current_path.all_used_namespaces();
                current_uses.contains(&name)
            },

            None => false,
        }
    }
}

fn visit_member<Predicate, Visitor>(
    member_path: &mut IdentPath,
    member: &ScopeMember,
    predicate: &Predicate,
    visitor: &mut Visitor,
) where
    Visitor: FnMut(&IdentPath, &Decl),
    Predicate: Fn(&IdentPath, &ScopeMember) -> bool,
{
    if !predicate(member_path, member) {
        return;
    }

    match member {
        ScopeMember::Decl(value) => {
            visitor(member_path, value);
        },

        ScopeMember::Scope(ns) => {
            let mut child_path = unsafe { IdentPath::empty() };
            mem::swap(&mut child_path, member_path);

            let mut child_member_path_parts = child_path.into_vec();

            for (key, member) in ns.members() {
                child_member_path_parts.push(key.clone());
                let mut child_member_path = IdentPath::from_vec(child_member_path_parts);
                visit_member(&mut child_member_path, member, predicate, visitor);

                child_member_path_parts = child_member_path.into_vec();
                child_member_path_parts.pop();
            }

            child_path = IdentPath::from_vec(child_member_path_parts);

            mem::swap(member_path, &mut child_path);
        },
    }
}

fn print_scope(ns: &Scope, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
    for _ in 0..indent {
        write!(f, " ")?;
    }

    match ns.key() {
        Some(key) => writeln!(f, "{}: [", key)?,
        None => writeln!(f, "(anon): [")?,
    }

    let members = ns
        .keys()
        .into_iter()
        .map(|key| ns.get_member(&key).unwrap());

    let member_indent = indent + 4;
    for (k, v) in members {
        match v {
            ScopeMember::Scope(child) => {
                print_scope(child, member_indent, f)?;
            },

            ScopeMember::Decl(val) => {
                for _ in 0..member_indent {
                    write!(f, " ")?;
                }

                writeln!(f, "{}: {}", k, val)?;
            },
        }
    }

    for _ in 0..indent {
        write!(f, " ")?;
    }
    writeln!(f, "]")
}

impl fmt::Display for ScopeStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[")?;

        for ns in &self.scopes {
            print_scope(ns, 4, f)?;
        }

        write!(f, "]")
    }
}
