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
use crate::codegen::builder::Builder;
use crate::codegen::expr::*;
use crate::codegen::metadata::*;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::stmt::*;
use crate::ast as syn;
use crate::typ as typ;
pub use ir_lang as ir;
use crate::ast::StructKind;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    // insert IR comments indicating RC release/retain operations
    pub annotate_rc: bool,

    // insert source spans for statements and expressions for improved error messaging in the
    // translation/interpreter stage
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

    let builtin_disposable = typ::builtin_disposable_iface();

    // make sure frontend builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut lib);
        let disposable_iface = builder.translate_iface(&builtin_disposable);
        builder.finish();

        disposable_iface
    };

    lib.metadata_mut().define_iface(disposable_iface);

    translate_builtin_class(&module, &mut lib, &typ::builtin_string_name(), ir::STRING_ID);
    translate_builtin_class(&module, &mut lib, &typ::builtin_typeinfo_name(), ir::TYPEINFO_ID);
    translate_builtin_class(&module, &mut lib, &typ::builtin_methodinfo_name(), ir::METHODINFO_ID);

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
