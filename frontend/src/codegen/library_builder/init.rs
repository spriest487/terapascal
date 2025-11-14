use terapascal_ir::instruction_builder::InstructionBuilder;
use crate::codegen::builder::IRBuilder;
use crate::codegen::expr::expr_to_val;
use crate::codegen::library_builder::LibraryBuilder;
use crate::typ as typ;
use crate::ir;

pub fn gen_tags_init(lib: &mut LibraryBuilder) -> Option<ir::FunctionID> {
    let locations: Vec<_> = lib
        .tags()
        .map(|(loc, tags)| (loc, tags.to_vec()))
        .collect();
    
    let mut builder = IRBuilder::new(lib);

    // type of field that stores tags in TypeInfo/MethodInfo class (array of object)
    for (loc, tags) in locations {
        let array_ref = ir::Ref::Global(ir::GlobalRef::StaticTagArray(loc));
        gen_create_tags(&mut builder, array_ref, tags);
    }
    
    let body = builder.finish();
    if body.is_empty() {
        return None;
    }
    
    let name = "<generated tag init function>".to_string();

    let sig = ir::FunctionSig::new([], ir::Type::Nothing);
    let func = ir::Function::new_local_def(Some(name), sig, body);

    let func_id = lib.metadata_mut().insert_func(None, false);
    lib.insert_function(func_id, func);
    
    Some(func_id)
}

fn gen_create_tags(
    builder: &mut IRBuilder,
    tag_array: ir::Ref,
    tags: Vec<typ::ast::TagItem>,
) {
    if tags.is_empty() {
        return;
    }

    let tag_array_ty = ir::ANY_TYPE.dyn_array();

    // var to store ref to each element (ref to field of type *any)
    let element_ref = builder.local_temp(ir::ANY_TYPE.temp_ref());

    for i in 0..tags.len() {
        let tag_item = &tags[i];

        // should be safe, tags must be translated before their owning types
        let tag_ty = builder.translate_type(&tag_item.tag_type)
            .rc_resource_def_id()
            .expect("tags must be classes!");

        let tag_class = tag_ty.to_class_ptr_type();

        let tag_struct_def = builder
            .get_struct(tag_ty)
            .expect("tag class struct must exist")
            .clone();

        let tag_instance = builder.local_temp(tag_class.clone());
        builder.rc_new(tag_instance, tag_ty, true);
        
        let index_val = ir::Value::LiteralI32(i as i32);
        builder.element(element_ref, tag_array.clone(), index_val, ir::ANY_TYPE, tag_array_ty.clone());
        builder.cast(element_ref.to_deref(), tag_instance, ir::ANY_TYPE);

        for arg in tag_item.args.members.iter() {
            let Some(field_id) = tag_struct_def.find_field(arg.ident.as_str()) else {
                continue;
            };

            let field_ty = &tag_struct_def.fields[&field_id].ty;
            let field_ref = builder.local_temp(field_ty.clone().temp_ref());
            builder.field(field_ref, tag_instance, tag_class.clone(), field_id);

            let arg_val = expr_to_val(&arg.value, builder);
            builder.mov(field_ref.to_deref(), arg_val);
        }
    }
}
