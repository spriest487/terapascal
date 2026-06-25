use crate::codegen::builder::IRBuilder;
use crate::codegen::expr::expr_to_val;
use crate::codegen::expr::translate_expr;
use crate::codegen::set_word_count;
use crate::codegen::WORD_TYPE;
use crate::ir;
use crate::typ;
use bigdecimal::BigDecimal;
use bigdecimal::Zero;
use std::sync::Arc;
use terapascal_ir::InstructionBuilder as _;
use terapascal_ir::MetadataSource as _;

pub fn build_object_ctor_invocation(
    object_ty: &typ::Type,
    members: &[typ::ast::ObjectCtorMember],
    builder: &mut IRBuilder
) -> ir::Ref {
    let object_ty = builder.translate_type(object_ty);

    let type_id = match &object_ty {
        ir::Type::Struct(id) => id,
        ir::Type::Object(ir::ObjectID::Class(id)) => id,
        _ => panic!("type of object ctor expr must be a record or class"),
    };

    let struct_def = builder
        .metadata()
        .instantiate_struct_def(type_id.def_id, &type_id.args)
        .unwrap_or_else(|| {
            panic!("target type {} referenced in object ctor must exist", object_ty)
        })
        .into_owned();

    // either local struct of the correct type for value types, or a rc pointer to the struct
    // type for rc class types
    let out_val = builder.local_var(object_ty.clone(), None).to_ref();

    if object_ty.is_object() {
        // allocate class struct at out pointer
        builder.new_object(out_val.clone(), type_id.def_id, type_id.args.to_vec(), false);
    }

    builder.scope(|builder| {
        for (field_id, field_def) in &struct_def.fields {
            let field_ref = out_val.clone().field_ref(object_ty.clone(), *field_id);

            match field_def.name
                .as_ref()
                .and_then(|field_name| {
                    members
                        .iter()
                        .find(|m| *m.ident.name == *field_name)
                })
            {
                Some(member) => {
                    let member_val = translate_expr(&member.value, builder);

                    builder.comment(format!(
                        "{}: {} ({})",
                        member.ident, member.value, field_def.ty
                    ));

                    builder.mov(field_ref.to_deref(), member_val);
                    if field_def.ty.contains_any_object_refs(builder.metadata()) {
                        builder.retain(field_ref.to_deref(), field_def.ty.clone());
                    }
                }

                // if the field isn't present in the constructor expression, initialize it with
                // the type's default value
                None => {
                    let field_display = match &field_def.name {
                        None => format!("field {}", field_id),
                        Some(name) => name.clone(),
                    };
                    builder.comment(format!("{field_display}: <no value>"));

                    builder.mov(field_ref.to_deref(), ir::Value::Default(field_def.ty.clone()));
                }
            }
        }
    });

    out_val
}

pub fn translate_object_ctor(ctor: &typ::ast::ObjectCtor, builder: &mut IRBuilder) -> ir::Ref {
    let object_ty = ctor.annotation.ty();
    
    build_object_ctor_invocation(object_ty.as_ref(), &ctor.args.members, builder)
}

pub fn translate_collection_ctor(ctor: &typ::ast::CollectionCtor, builder: &mut IRBuilder) -> ir::Ref {
    let ctor_ty = ctor.annotation.ty();
    match ctor_ty.as_ref() {
        typ::Type::Array(array_ty) => {
            translate_static_array_ctor(ctor, &array_ty.element_ty, array_ty.dim, builder)
        },

        typ::Type::DynArray(element) => {
            translate_dyn_array_ctor(ctor, element, builder)
        },

        typ::Type::Set(set_type) => {
            let flags_type = builder.translate_type(ctor_ty.as_ref());
            translate_set_ctor(ctor, set_type, flags_type, builder)
        }
        
        typ::Type::Box(value_ty) => {
            translate_box_ctor(ctor, value_ty, builder)
        }

        unimpl => unimplemented!("IR for collection constructor {} of type {}", ctor, unimpl),
    }
}

fn translate_static_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    dim: usize,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let el_ty = builder.translate_type(element);

    let array_ty = el_ty.clone().array(dim);
    let arr = builder.local_var(array_ty.clone(), None);

    if dim > 0 {
        builder.scope(|builder| {
            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    let index = i32::try_from(i).expect("invalid array index in array ctor");
                    let index_val = ir::Value::LiteralI32(index);

                    let element_ref = arr.to_ref().element_ref(array_ty.clone(), index_val);
                    let el_init = translate_expr(&el.value, builder);

                    builder.mov(element_ref.to_deref(), el_init);
                    builder.retain(element_ref.to_deref(), el_ty.clone());
                });
            }
        });
    }

    arr.to_ref()
}

fn translate_dyn_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let elem_ty = builder.translate_type(element);

    let mut elements = Vec::with_capacity(ctor.elements.len());

    for element in &ctor.elements {
        let element_val = translate_expr(&element.value, builder);
        elements.push(element_val.value());
    }

    builder.new_array_from(elements, &elem_ty)
}

fn translate_set_ctor(
    ctor: &typ::ast::CollectionCtor,
    set_type: &Arc<typ::SetDef>,
    flags_type: ir::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let set_result = builder.local_var(flags_type.clone(), None);

    // zero-init the value
    let zero_word = ir::Value::from_literal_val(BigDecimal::zero(), &WORD_TYPE).unwrap();

    let set_flags_type = builder.translate_set_type(set_type);
    let set_flags_ref = set_flags_type.flags_ref(set_result);

    for word in 0..set_word_count(set_type.flags_type_bits()) {
        let word_field_ref = set_flags_type.repr_type.word_ref(set_flags_ref.clone(), word);
        builder.mov(word_field_ref, zero_word.clone());
    }
    
    let item_type = builder.translate_type(&set_type.item_type);

    let set_min = BigDecimal::from(set_type.min.as_i128());
    let min_val = ir::Value::from_literal_val(set_min.clone(), &item_type)
        .unwrap_or_else(|| panic!("set value type must be a valid numeric primitive, was: {} ({})", set_min, item_type));
    
    let bit = builder.local_temp(ir::Type::U8);

    for item in &ctor.elements {
        let item_val = translate_expr(&item.value, builder);
        let item_index = builder.sub_to_val(item_val, min_val.clone(), &item_type);

        builder.cast(bit, item_index, ir::Type::U8);
        builder.set_include(set_result, bit, set_type);
    }

    set_result.to_ref()
}

fn translate_box_ctor(
    ctor: &typ::ast::CollectionCtor,
    value_ty: &typ::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let value = expr_to_val(&ctor.elements[0].value, builder);
    
    let boxed_ty = builder.translate_type(value_ty);
    let box_ty = boxed_ty.clone().boxed();
    
    let box_var = builder.local_var(box_ty.clone(), None);
    
    builder.new_box(box_var, boxed_ty.clone(), false);

    builder.local_begin();
    {
        let element_ref = box_var.to_ref().element_ref(box_ty, ir::Value::I32_0);
        builder.mov(element_ref.to_deref(), value);
        builder.retain(element_ref.to_deref(), boxed_ty);
    }
    builder.local_end();

    box_var.to_ref()
}
