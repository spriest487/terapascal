use crate::ast;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::Module;
use crate::typ::ModuleUnit;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeResult;
use terapascal_common::read_source_file;
use std::collections::HashMap;
use std::iter;
use std::path::PathBuf;
use terapascal_common::span::Spanned;

const INT32: Type = Type::Primitive(Primitive::Int32);
const BOOL: Type = Type::Primitive(Primitive::Boolean);

pub fn try_module_from_src(unit_name: &str, src: &str) -> TypeResult<Module> {
    try_module_from_srcs(vec![(unit_name, src)])
}

pub fn module_from_src(unit_name: &str, src: &str) -> Module {
    module_from_srcs(vec![(unit_name, src)])
}

pub fn try_module_from_srcs<'a, UnitSources>(unit_srcs: UnitSources) -> TypeResult<Module>
where
    UnitSources: IntoIterator<Item = (&'a str, &'a str)>,
{
    let mut units = Vec::new();

    // always include the system unit from the configure unit path
    let unit_path = PathBuf::from(env!("TERAPASCAL_UNITS"));
    let system_src = read_source_file(&unit_path.join("System.tpas")).unwrap();

    let unit_srcs = iter::once(("System", system_src)).chain(
        unit_srcs
            .into_iter()
            .map(|(unit_name, unit_src)| (unit_name, unit_src.to_string())),
    );

    for (unit_name, src) in unit_srcs {
        let unit = ast::util::unit_from_string(unit_name, &src);

        units.push(unit);
    }

    Module::typecheck(&units, false)
}

pub fn module_from_srcs<'a, UnitSources>(unit_srcs: UnitSources) -> Module
where
    UnitSources: IntoIterator<Item = (&'a str, &'a str)>,
{
    try_module_from_srcs(unit_srcs)
        .unwrap_or_else(|err| panic!("{}\n{}", err, err.span()))
}

pub fn unit_from_src(unit_name: &'static str, src: &'static str) -> ModuleUnit {
    try_unit_from_src(unit_name, src)
        .unwrap_or_else(|err| panic!("{}\n{}", err, err.span()))
}

pub fn try_unit_from_src(unit_name: &'static str, src: &'static str) -> TypeResult<ModuleUnit> {
    let mut module = try_module_from_src(unit_name, src)?;

    Ok(module.units.pop().unwrap())
}

pub fn units_from_src<UnitSources>(unit_srcs: UnitSources) -> HashMap<String, ModuleUnit>
where
    UnitSources: IntoIterator<Item = (&'static str, &'static str)>,
{
    let module = module_from_srcs(unit_srcs);

    let mut units = HashMap::new();
    for unit in module.units {
        units.insert(unit.unit.ident.to_string(), unit);
    }

    units
}

#[test]
fn sig_without_self_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: BOOL,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_no_params_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![FunctionSigParam::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![FunctionSigParam::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_and_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![FunctionSigParam::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![FunctionSigParam::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![FunctionSigParam::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![FunctionSigParam::by_val(BOOL)],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}
