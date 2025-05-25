mod builder;
mod expr;
pub mod metadata;
mod stmt;
mod pattern;
mod function;
pub mod library_builder;
mod set_flags;

pub use self::function::*;
pub use self::set_flags::*;
use crate::ast as syn;
use crate::ast::StructKind;
use crate::codegen::builder::Builder;
use crate::codegen::expr::*;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::metadata::*;
use crate::codegen::stmt::*;
use crate::ir;
use crate::typ as typ;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    // insert IR comments indicating RC release/retain operations
    pub annotate_rc: bool,

    // insert source spans for statements and expressions for improved error messaging in the
    // translation/vm stage
    pub debug: bool,
}

impl Default for IROptions {
    fn default() -> Self {
        Self {
            annotate_rc: false,

            debug: true,
        }
    }
}

pub fn gen_lib(module: &typ::Module, opts: IROptions) -> ir::Library {
    let metadata = ir::Metadata::new();
    let mut lib = LibraryBuilder::new((*module.root_ctx).clone(), metadata, opts);

    translate_builtin_class(&module, &mut lib, &typ::builtin_string_name(), ir::STRING_ID);
    translate_builtin_class(&module, &mut lib, &typ::builtin_typeinfo_name(), ir::TYPEINFO_ID);
    translate_builtin_class(&module, &mut lib, &typ::builtin_methodinfo_name(), ir::METHODINFO_ID);
    translate_builtin_class(&module, &mut lib, &typ::builtin_funcinfo_name(), ir::FUNCINFO_ID);

    for unit in &module.units {
        lib.translate_unit(&unit.unit);
    }

    lib.finish()
}

fn translate_builtin_class(
    module: &typ::Module,
    lib: &mut LibraryBuilder,
    name: &typ::Symbol,
    id: ir::TypeDefID
) {
    let Ok(class_def) = module.root_ctx
        .find_struct_def(&name.full_path, StructKind::Class) else
    {
        return;
    };
    
    let generic_ctx = typ::GenericContext::empty();

    let name = translate_name(name, &generic_ctx, lib);

    lib.metadata_mut().declare_struct(id, &name, true);

    let resource_ty = translate_struct_def(class_def.as_ref(), &generic_ctx, lib);

    lib.metadata_mut().define_struct(id, resource_ty);
    lib.gen_runtime_type(&ir::Type::Struct(id));
    lib.gen_runtime_type(&ir::Type::RcPointer(ir::VirtualTypeID::Class(id)));
}
