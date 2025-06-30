use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::typ::Scope;
use crate::typ::ScopeMember;
use crate::typ::ScopeMemberRef;

#[derive(Debug)]
pub struct ScopePathRef<'s> {
    pub(super) scopes: Vec<&'s Scope>,
}

impl<'s> Clone for ScopePathRef<'s> {
    fn clone(&self) -> Self {
        ScopePathRef {
            scopes: self.scopes.clone(),
        }
    }
}

impl<'s> ScopePathRef<'s> {
    pub fn rev(&self) -> impl Iterator<Item=&'s Scope> + '_ {
        self.scopes.iter().cloned().rev()
    }
    
    pub fn find(&self, key: &Ident) -> Option<ScopeMemberRef<'s>> {
        let mut current = self.scopes.len() - 1;

        loop {
            let scope = &self.scopes[current];
            let path = &self.scopes[0..=current];

            if let Some((key, member)) = scope.get_member(key) {
                return match member {
                    ScopeMember::Scope(ns) => {
                        let mut path = path.to_vec();
                        path.push(ns);

                        Some(ScopeMemberRef::Scope {
                            path: ScopePathRef { scopes: path },
                        })
                    }

                    ScopeMember::Decl(value) => {
                        let parent_path = ScopePathRef { scopes: path.to_vec() };

                        Some(ScopeMemberRef::Decl {
                            key,
                            value,
                            parent_path,
                        })
                    },
                }
            }

            if scope.key() == Some(key) {
                return Some(ScopeMemberRef::Scope {
                    path: ScopePathRef { scopes: path.to_vec() },
                });
            }

            if current == 0 {
                break;
            }

            current -= 1;
            // while current > 0 && !scope.can_access_members(self.namespaces[current]) {
            //     current -= 1;
            // }
        }

        None
    }

    pub fn as_slice(&self) -> &[&Scope] {
        self.scopes.as_slice()
    }

    pub fn top(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn keys(&self) -> impl Iterator<Item = &Ident> {
        self.scopes.iter().cloned().filter_map(|s| s.key())
    }

    pub fn join(&self, sep: &str) -> String {
        self.scopes
            .iter()
            .filter_map(|ns| ns.key())
            .map(|k| k.to_string())
            .collect::<Vec<_>>()
            .join(sep)
    }

    pub fn to_namespace(&self) -> IdentPath {
        let namespace = self
            .as_slice()
            .iter()
            .filter_map(|s| s.key().cloned());

        IdentPath::from_parts(namespace)
    }

    pub fn all_used_namespaces(&self) -> Vec<&IdentPath> {
        let mut namespaces = Vec::new();

        for scope in self.as_slice().iter().rev() {
            for used_ns in scope.use_namespaces() {
                if !namespaces.contains(&used_ns) {
                    namespaces.push(used_ns);
                }
            }
        }

        namespaces
    }

    pub fn is_used_unit(&self, unit_name: &IdentPath) -> bool {
        self.as_slice()
            .iter()
            .rev()
            .flat_map(|scope| scope.use_namespaces.iter())
            .any(|used_unit| used_unit == unit_name)
    }

    pub fn is_parent_of(&self, other: &ScopePathRef) -> bool {
        if self.scopes.len() >= other.scopes.len() {
            return false;
        }

        other.scopes
            .iter()
            .zip(self.scopes.iter())
            .all(|(a, b)| a.id == b.id)
    }
}

#[derive(Debug)]
pub struct ScopePathRefMut<'s> {
    pub(super) namespaces: Vec<&'s mut Scope>,
}

impl<'s> ScopePathRefMut<'s> {
    pub fn top(&'s mut self) -> &'s mut Scope {
        self.namespaces.last_mut().unwrap()
    }

    pub fn as_slice(&mut self) -> &mut [&'s mut Scope] {
        self.namespaces.as_mut_slice()
    }
}
