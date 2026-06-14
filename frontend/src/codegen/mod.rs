mod builder;
mod expr;
pub mod metadata;
mod stmt;
mod pattern;
mod function;
pub mod library_builder;
mod set_flags;

use self::builder::IRBuilder;
use self::expr::*;
pub use self::function::*;
use self::library_builder::LibraryBuilder;
use self::metadata::*;
pub use self::set_flags::*;
use self::stmt::*;
use crate::ast::StructKind;
use crate::codegen::library_builder::LibraryRef;
use crate::ir;
use crate::typ as typ;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CodegenOpts {
    // insert source spans for statements and expressions for improved error messaging in the
    // translation/vm stage
    pub debug: bool,
    
    pub rtti: bool,
}

impl Default for CodegenOpts {
    fn default() -> Self {
        Self {
            debug: true,
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
    let Ok(class_def) = type_ctx.find_struct_def(&name.full_path, StructKind::Class)
    else {
        return;
    };

    let name = translate_name(name, lib);

    lib.metadata_mut().declare_type(id, &name);

    let resource_ty = translate_struct_def(class_def.as_ref(), lib);

    lib.metadata_mut().define_struct(id, resource_ty);

    // assume builtin classes don't have type params
    lib.gen_type_info(&id.to_class_ptr_type([]));
    lib.gen_type_info(&id.to_class_weak_type([]));
}
