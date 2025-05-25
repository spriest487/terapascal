use crate::ast::Literal;
use crate::typ::Decl;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Scope;
use crate::typ::ScopeMemberKind;
use crate::typ::ScopeMemberRef;
use crate::typ::ScopeStack;
use crate::typ::Type;
use crate::typ::ScopeID;
use terapascal_common::span::Span;
use crate::ast::Visibility;
use crate::ast::Ident;
use crate::ast::IdentPath;

fn new_global_scope() -> ScopeStack {
    ScopeStack::new(Scope::new(ScopeID(0), Environment::Global))
}

fn new_scope(name: &str) -> Scope {
    let namespace = IdentPath::from_parts(vec![ident(name)]);

    Scope::new(ScopeID(0), Environment::Namespace { namespace })
}

fn new_scope_anon() -> Scope {
    Scope::new(
        ScopeID(0),
        Environment::Block {
            allow_unsafe: false,
        },
    )
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::zero("test"))
}

fn const_decl(val: i32) -> Decl {
    Decl::GlobalConst {
        ty: Type::Primitive(Primitive::Int32),
        span: Span::zero("test"),
        val: Literal::Integer(val.into()),
        visibility: Visibility::Interface,
    }
}

fn resolve_path<'a>(namespaces: &ScopeStack, path: impl IntoIterator<Item=&'a str>) -> ScopeMemberRef {
    let path_parts = path.into_iter()
        .map(ident);

    namespaces.resolve_path(&IdentPath::from_parts(path_parts)).unwrap()
}

#[test]
fn finds_val_in_same_ctx() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key()
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn finds_val_in_parent_ctx() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();
    namespaces.push_scope(new_scope("B"));

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop_scope();
}

#[test]
fn finds_top_val_of_shadowed_name() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(321)).unwrap();
    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop_scope();
}

#[test]
fn finds_val_in_anon_namespace() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.push_scope(new_scope_anon());
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();
    namespaces.push_scope(new_scope("B"));

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(None, parent_path.as_slice().last().unwrap().key());
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop_scope();
    namespaces.pop_scope();
}

#[test]
fn finds_val_through_anon_namespace() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.push_scope(new_scope_anon());
    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key()
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }

    namespaces.pop_scope();
    namespaces.pop_scope();
}

#[test]
fn finds_val_in_declared_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();
    namespaces.pop_scope();

    let b = match namespaces.current_path().find(&ident("B")) {
        Some(ScopeMemberRef::Scope { path }) => path,
        _ => panic!("B should be a namespace"),
    };

    match b.find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_declared_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();
    namespaces.pop_scope();

    let path = IdentPath::from_parts([ident("B"), ident("x")]);

    match namespaces.resolve_path(&path) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_explicit_root_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("A"), ident("x")]);

    match namespaces.resolve_path(&path) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_root_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("x")]);

    match namespaces.resolve_path(&path) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_explicit_current_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("B"), ident("x")]);

    match namespaces.resolve_path(&path) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn resolves_val_in_current_ns() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));

    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    let path = IdentPath::from_parts([ident("x")]);

    match namespaces.resolve_path(&path) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("B")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(123));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn replaces_val_in_correct_scope() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();
    namespaces
        .replace_decl(ident("x"), const_decl(321))
        .unwrap();

    match namespaces.current_path().find(&ident("x")) {
        Some(ScopeMemberRef::Decl {
            value, parent_path, ..
        }) => {
            assert_eq!(
                Some(ident("A")).as_ref(),
                parent_path.as_slice().last().unwrap().key(),
            );
            assert_eq!(*value, const_decl(321));
        }
        _ => panic!("x should be a value"),
    }
}

#[test]
fn replace_returns_err_for_val_in_wrong_scope() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(123)).unwrap();

    namespaces.push_scope(new_scope("B"));

    assert!(namespaces
        .replace_decl(ident("x"), const_decl(321))
        .is_err());
}

#[test]
fn finds_popped_scope_by_key() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.push_scope(new_scope_anon());
    namespaces.push_scope(new_scope("B"));
    namespaces.pop_scope();

    match namespaces.current_path().find(&ident("B")) {
        Some(ScopeMemberRef::Scope { path }) => {
            let path_keys: Vec<_> = path.keys().cloned().collect();
            assert_eq!(&[ident("A"), ident("B")], path_keys.as_slice());
        }
        _ => panic!("B should be a namespace"),
    }
}

fn visit_all_to_vec(namespaces: &ScopeStack) -> Vec<(String, Decl)> {
    let mut visited = Vec::new();

    namespaces.visit_members(
        |_path, _member| true,
        |path, val| {
            visited.push((path.to_string(), val.clone()));
        },
    );

    visited
}

#[test]
fn visit_all_visits_parent_scopes_rev_order() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("a_val"), const_decl(1)).unwrap();
    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("b_val"), const_decl(2)).unwrap();

    let visited = visit_all_to_vec(&namespaces);

    assert_eq!(2, visited.len());
    assert_eq!(("A.B.b_val".to_string(), const_decl(2)), visited[0]);
    assert_eq!(("A.a_val".to_string(), const_decl(1)), visited[1]);
}

#[test]
fn visit_all_visits_unnamed_scopes() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope("A"));
    namespaces.insert_decl(ident("a_val"), const_decl(1)).unwrap();
    namespaces.push_scope(new_scope_anon());
    namespaces.insert_decl(ident("b_val"), const_decl(2)).unwrap();

    let visited = visit_all_to_vec(&namespaces);

    assert_eq!(2, visited.len());
    assert_eq!(("A.b_val".to_string(), const_decl(2)), visited[0]);
    assert_eq!(("A.a_val".to_string(), const_decl(1)), visited[1]);
}

#[test]
fn finds_deep_nested_child_decl() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope_anon());
    namespaces.push_scope(new_scope("A"));
    namespaces.push_scope(new_scope("B"));
    namespaces.push_scope(new_scope("C"));
    namespaces.insert_decl(ident("x"), const_decl(456)).unwrap();

    namespaces.pop_scope();
    namespaces.pop_scope();
    namespaces.pop_scope();

    let found = namespaces.resolve_path(&IdentPath::from_parts([
        ident("A"),
        ident("B"),
        ident("C"),
        ident("x")
    ])).unwrap();

    assert_eq!(Some(const_decl(456)).as_ref(), found.as_value())
}

#[test]
fn finds_sibling_scope_decl() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope_anon());
    namespaces.push_scope(new_scope("A"));
    namespaces.insert_decl(ident("x"), const_decl(523)).unwrap();
    namespaces.pop_scope();

    namespaces.push_scope(new_scope("B"));

    let found = namespaces.resolve_path(&IdentPath::from_parts([
        ident("A"),
        ident("x")
    ])).unwrap();

    assert_eq!(Some(const_decl(523)).as_ref(), found.as_value())
}

#[test]
fn find_fully_qualified_in_current_scope() {
    let mut namespaces: ScopeStack = ScopeStack::new(new_scope_anon());
    namespaces.push_scope(new_scope("A"));
    namespaces.push_scope(new_scope("B"));
    namespaces.insert_decl(ident("x"), const_decl(987)).unwrap();
    
    let found = resolve_path(&namespaces, ["A", "B", "x"]);

    assert_eq!(Some(const_decl(987)).as_ref(), found.as_value())
}

#[test]
fn allow_decl_with_same_name_as_enclosing_scope() {
    let mut namespaces = new_global_scope();
    namespaces.push_scope(new_scope("Test1"));
    let declare_const = namespaces.insert_decl(ident("Test1"), const_decl(123));
    
    assert!(declare_const.is_ok());
}

#[test]
fn finds_decl_before_current_path() {
    let mut namespaces = new_global_scope();
    namespaces.push_scope(new_scope("Test1"));
    namespaces.insert_decl(ident("Test1"), const_decl(123)).unwrap();
    namespaces.pop_scope();

    let test1_scope = resolve_path(&namespaces, ["Test1"]);
    assert_eq!(ScopeMemberKind::Scope, test1_scope.kind());
    
    let test1_decl = test1_scope.as_scope().unwrap().find(&ident("Test1"));
    assert!(test1_decl.is_some());
    assert_eq!(ScopeMemberKind::Decl, test1_decl.unwrap().kind());
}
