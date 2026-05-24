use crate::codegen::builder::IRBuilder;
use crate::codegen::library_builder::LibraryBuilder;
use crate::ir;
use std::rc::Rc;
use terapascal_ir::InstructionBuilder;
use terapascal_ir::MetadataSource;

pub fn gen_tags_init(lib: &mut LibraryBuilder) -> Option<ir::FunctionID> {
    let locations: Vec<_> = lib.metadata.all_tags()
        .map(|(loc, tags)| (loc, tags.to_vec()))
        .collect();
    
    let mut builder = IRBuilder::new(lib);

    // type of field that stores tags in TypeInfo/MethodInfo class (array of object)
    for (loc, tags) in locations {
        let array_ref = ir::Ref::Global(ir::GlobalRef::StaticTagArray(loc));
        gen_create_tags(&mut builder, array_ref, tags);
    }

    let body = builder.finish();
    if body.instructions.is_empty() {
        return None;
    }

    let internal_name = "generated tag init function".to_string();
    let debug_name = lib.opts.debug.then(|| internal_name.clone());
    let identity = ir::FunctionIdentity::internal(internal_name);

    let sig = ir::FunctionSig::new([], ir::Type::Nothing);
    let func = ir::Function::new_local_def(debug_name, Vec::new(), sig.clone(), body);

    let func_id = lib.metadata_mut().insert_func(identity, Rc::new(sig), false, []);
    lib.insert_function(func_id, func);
    
    Some(func_id)
}

fn gen_create_tags(
    builder: &mut IRBuilder,
    tag_array: ir::Ref,
    tags: Vec<ir::TagInfo>,
) {
    if tags.is_empty() {
        return;
    }

    let tag_array_ty = ir::ANY_TYPE.dyn_array();
    
    builder.scope(|builder| {
        for i in 0..tags.len() {
            let tag_info = &tags[i];

            let tag_class = tag_info.class_id.to_class_ptr_type([]);

            let tag_instance = builder.local_temp(tag_class.clone());
            builder.new_object(tag_instance, tag_info.class_id, [], true);

            let index_val = ir::Value::LiteralI32(i as i32);

            let element_ref = tag_array.clone().element_ref(tag_array_ty.clone(), index_val);
            builder.cast(element_ref.to_deref(), tag_instance, ir::ANY_TYPE);

            for (field_id, field_val) in tag_info.fields.iter() {
                let field_ref = tag_instance.to_ref().field_ref(tag_class.clone(), *field_id);

                builder.mov(field_ref.to_deref(), field_val.clone());
            }
        }
    });
}
