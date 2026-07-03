pub mod metadata;
pub mod library_builder;

mod builder;
mod expr;
mod stmt;
mod pattern;
mod function;
mod set_flags;
mod alias;
mod enum_type;
mod var_param;

pub use self::alias::*;
pub use self::function::*;
pub use self::set_flags::*;
pub use self::enum_type::*;
pub use self::var_param::*;

use self::builder::IRBuilder;
use self::expr::*;
use self::library_builder::LibraryBuilder;
use self::metadata::*;
use self::stmt::*;
use crate::ast::StructKind;
use crate::codegen::library_builder::LibraryRef;
use crate::ir;
use crate::typ as typ;
use ir::MetadataSource as _;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CodegenOpts {
    // insert source spans for statements and expressions for improved error messaging in the
    // translation/vm stage
    pub debug: bool,
    
    pub no_system: bool,
    
    pub rtti: bool,
}

impl Default for CodegenOpts {
    fn default() -> Self {
        Self {
            debug: true,
            no_system: false,
            rtti: true,
        }
    }
}

pub fn gen_lib(
    module: &typ::Module,
    type_ctx: &typ::Context,
    refs: &[LibraryRef],
    opts: CodegenOpts,
) -> ir::Library {
    let mut lib = LibraryBuilder::new(module.name.as_str(), module.version, type_ctx, refs, opts);

    // if the system package is excluded, these essential types need to be defined in this library.
    // normally these defs are loaded from the precompiled system library or, when compiling the
    // system unit itself, superseded by the defs there
    translate_builtin_class(&mut lib, type_ctx, &typ::builtin_string_name(), ir::STRING_ID);

    if opts.rtti {
        translate_builtin_class(&mut lib, type_ctx, &typ::builtin_typeinfo_name(), ir::TYPEINFO_ID);
        translate_builtin_class(&mut lib, type_ctx, &typ::builtin_methodinfo_name(), ir::METHODINFO_ID);
        translate_builtin_class(&mut lib, type_ctx, &typ::builtin_funcinfo_name(), ir::FUNCINFO_ID);
    }

    for unit in &module.units {
        lib.translate_unit(&unit.unit);
    }

    lib.finish()
}

fn translate_builtin_class(
    lib: &mut LibraryBuilder,
    type_ctx: &typ::Context,
    name: &typ::Symbol,
    id: ir::TypeDefID
) {
    // definition must exist in the source module - we can assume builtins always are
    let Ok(class_def) = type_ctx.find_struct_def(&name.full_path, StructKind::Class)
    else {
        return;
    };

    // if there's already a definition loaded, e.g. from a referenced library, skip this item
    if lib.metadata().get_type_def(id).is_some() {
        return;
    }

    let def_path = translate_name(name, lib);

    lib.metadata_mut().declare_type(id, &def_path);

    let resource_ty = translate_struct_def(class_def.as_ref(), lib);

    lib.metadata_mut().define_struct(id, resource_ty);

    // assume builtin classes don't have type params
    lib.gen_type_info(&id.to_class_ptr_type([]), &name.to_string());
}
