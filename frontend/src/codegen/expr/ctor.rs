use crate::codegen::builder::IRBuilder;
use crate::codegen::expr;
use crate::codegen::expr::{expr_to_val, translate_expr};
use crate::codegen::set_word_count;
use crate::codegen::WORD_TYPE;
use crate::ir;
use crate::typ;
use bigdecimal::BigDecimal;
use bigdecimal::Zero;
use terapascal_ir::InstructionBuilder as _;

pub fn build_object_ctor_invocation(
    object_ty: &typ::Type,
    members: &[typ::ast::ObjectCtorMember],
    builder: &mut IRBuilder
) -> ir::Ref {
    let object_ty = builder.translate_type(object_ty);

    let struct_id = match &object_ty {
        ir::Type::Object(ir::ObjectID::Class(struct_id)) => *struct_id,
        ir::Type::Struct(struct_id) => *struct_id,
        _ => panic!("type of object ctor expr must be a record or class"),
    };

    let struct_def = builder
        .get_struct(struct_id)
        .unwrap_or_else(|| {
            panic!("target type {} referenced in object ctor must exist", object_ty)
        })
        .clone();

    // either local struct of the correct type for value types, or a rc pointer to the struct
    // type for rc class types
    let out_val = builder.local_var(object_ty.clone(), None).to_ref();

    if object_ty.is_object() {
        // allocate class struct at out pointer
        builder.new_object(out_val.clone(), struct_id, false);
    }

    builder.scope(|builder| {
        for member in members {
            let member_val = expr::translate_expr(&member.value, builder);
            let field_id = struct_def
                .find_field(&member.ident.name)
                .unwrap_or_else(|| {
                    panic!(
                        "field {} referenced in object ctor must exist",
                        member.ident
                    )
                });

            let field_def = struct_def.get_field(field_id).unwrap();

            builder.comment(&format!(
                "{}: {} ({})",
                member.ident, member.value, field_def.ty
            ));

            let field_ref = builder.local_temp(field_def.ty.clone().temp_ref());
            builder.field(field_ref, out_val.clone(), object_ty.clone(), field_id);

            builder.mov(field_ref.to_deref(), member_val);
            builder.retain_deep(field_ref.to_deref(), &field_def.ty);
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
            let element_ref = builder.local_temp(el_ty.clone().temp_ref());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    let index = i32::try_from(i).expect("invalid array index in array ctor");
                    let index_val = ir::Value::LiteralI32(index);

                    builder.element(element_ref, arr, index_val, array_ty.clone());

                    let el_init = translate_expr(&el.value, builder);

                    builder.mov(element_ref.to_deref(), el_init);
                    builder.retain_deep(element_ref.to_deref(), &el_ty);
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

    builder.new_dyn_array(elements, &elem_ty)
}

fn translate_set_ctor(
    ctor: &typ::ast::CollectionCtor,
    set_type: &typ::SetType,
    flags_type: ir::Type,
    builder: &mut IRBuilder
) -> ir::Ref {
    let set_result = builder.local_var(flags_type.clone(), None);

    // zero-init the value
    let word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());
    let zero_word = ir::Value::from_literal_val(BigDecimal::zero(), &WORD_TYPE).unwrap();

    for word in 0..set_word_count(set_type.flags_type_bits()) {
        builder.field(word_field_ref, set_result, flags_type.clone(), ir::FieldID(word));
        builder.mov(word_field_ref.to_deref(), zero_word.clone());
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
        let element_ref = builder.local_temp(boxed_ty.temp_ref());
        builder.element(element_ref, box_var, ir::Value::LiteralI32(0), box_ty);
        builder.mov(element_ref.to_deref(), value);
    }
    builder.local_end();
    
    box_var.to_ref()
}
