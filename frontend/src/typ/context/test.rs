use crate::ast::Visibility;
use crate::typ::Context;
use crate::typ::Symbol;
use crate::typ::Type;
use terapascal_common::ident::Ident;
use terapascal_common::ident::IdentPath;
use terapascal_common::span::Span;
use terapascal_common::CompileOpts;

#[test]
pub fn find_name_does_not_find_invisible_member() {
    let mut ctx = Context::root(CompileOpts::default());

    let span = Span::zero("test");
    let unit_a_name = IdentPath::new(Ident::new("ScopeA", span.clone()), []);
    let unit_b_name = IdentPath::new(Ident::new("ScopeB", span.clone()), []);

    let class_ident = Ident::new("PrivateClass", span.clone());
    let class_path = unit_a_name.clone().child(class_ident.clone());

    let unit_a_scope = ctx.push_unit_scope(unit_a_name.clone()).unwrap();
    ctx.declare_type(class_ident.clone(), Type::class(Symbol::from(class_path)), Visibility::Implementation, false);
    ctx.pop_scope(unit_a_scope);

    ctx.push_unit_scope(unit_b_name).unwrap();
    ctx.use_namespace(&unit_a_name);
    let result = ctx.find_name(&class_ident, false);

    if let Some(unexpected) = result {
        panic!("expected to find None, found: {:?}", unexpected);
    }
}

#[test]
pub fn find_path_does_not_find_invisible_member() {
    let mut ctx = Context::root(CompileOpts::default());

    let span = Span::zero("test");
    let unit_a_name = IdentPath::new(Ident::new("ScopeA", span.clone()), []);
    let unit_b_name = IdentPath::new(Ident::new("ScopeB", span.clone()), []);

    let class_ident = Ident::new("PrivateClass", span.clone());
    let class_path = unit_a_name.clone().child(class_ident.clone());

    let unit_a_scope = ctx.push_unit_scope(unit_a_name.clone()).unwrap();
    ctx.declare_type(class_ident.clone(), Type::class(Symbol::from(class_path.clone())), Visibility::Implementation, false);
    ctx.pop_scope(unit_a_scope);

    ctx.push_unit_scope(unit_b_name).unwrap();
    ctx.use_namespace(&unit_a_name);
    let result = ctx.find_path(&class_path, false);

    if let Some(unexpected) = result {
        panic!("expected to find None, found: {:?}", unexpected);
    }
}