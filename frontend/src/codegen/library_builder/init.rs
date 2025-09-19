use terapascal_ir::instruction_builder::InstructionBuilder;
use crate::codegen::builder::Builder;
use crate::codegen::expr::expr_to_val;
use crate::codegen::library_builder::LibraryBuilder;
use crate::typ as typ;
use crate::ir;

pub fn gen_tags_init(lib: &mut LibraryBuilder) -> Option<ir::FunctionID> {
    let locations: Vec<_> = lib
        .tags()
        .map(|(loc, tags)| (loc, tags.to_vec()))
        .collect();
    
    let mut builder = Builder::new(lib);

    // type of field that stores tags in TypeInfo/MethodInfo class (array of object)
    let any_dyn_array = builder.translate_dyn_array_struct(&typ::Type::Any);
    let any_dyn_array_ty = ir::Type::class_ptr(any_dyn_array);

    for (loc, tags) in locations {
        let array_ref = ir::Ref::Global(ir::GlobalRef::StaticTagArray(loc));
        gen_create_tags(&mut builder, array_ref, tags, &any_dyn_array_ty);
    }
    
    let body = builder.finish();
    if body.is_empty() {
        return None;
    }
    
    let name = "<generated tag init function>".to_string();

    let sig = ir::FunctionSig::new([], ir::Type::Nothing);
    let func = ir::Function::new_local_def(Some(name), sig, body);

    let func_id = lib.metadata_mut().insert_func(None);
    lib.insert_func(func_id, func);
    
    Some(func_id)
}

fn gen_create_tags(
    builder: &mut Builder,
    tag_array: ir::Ref,
    tags: Vec<typ::ast::TagItem>,
    tag_array_ty: &ir::Type
) {
    if tags.is_empty() {
        return;
    }

    // get the ptr field of the tags array (pointer to field of type Object^)
    let tags_array_ptr_field_ptr = builder.local_temp(ir::ANY_TYPE.ptr().ptr());
    builder.field(tags_array_ptr_field_ptr.clone(), tag_array, tag_array_ty.clone(), ir::DYNARRAY_PTR_FIELD);

    let tag_ptr = builder.local_temp(ir::ANY_TYPE.ptr());

    for i in 0..tags.len() {
        let tag_item = &tags[i];

        // should be safe, tags must be translated before their owning types

        let tag_ty = builder.translate_type(&tag_item.tag_type)
            .rc_resource_def_id()
            .expect("tags must be classes!");

        let tag_class = ir::Type::class_ptr(tag_ty);

        let tag_struct_def = builder
            .get_struct(tag_ty)
            .expect("tag class struct must exist")
            .clone();

        // deref the field to get the array pointer, offset it to get the item pointer
        // tag_ptr := (tags_array_ptr_field_ptr^) + (i * sizeof(Any)
        builder.add(tag_ptr.clone(), tags_array_ptr_field_ptr.clone().to_deref(), ir::Value::LiteralI32(i as i32));

        let tag_instance = builder.local_temp(tag_class.clone());
        builder.rc_new(tag_instance.clone(), tag_ty, true);

        // instantiate the tag class
        builder.mov(tag_ptr.clone().to_deref(), tag_instance.clone());

        for arg in tag_item.args.members.iter() {
            let Some(field_id) = tag_struct_def.find_field(arg.ident.as_str()) else {
                continue;
            };

            let field_ty = &tag_struct_def.fields[&field_id].ty;
            let field_ptr = builder.local_temp(field_ty.clone().ptr());
            builder.field(field_ptr.clone(), tag_instance.clone(), tag_class.clone(), field_id);

            let arg_val = expr_to_val(&arg.value, builder);
            builder.mov(field_ptr.to_deref(), arg_val);
        }
    }
}
