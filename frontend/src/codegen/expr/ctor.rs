use crate::codegen::builder::Builder;
use crate::codegen::{expr, set_word_count, WORD_TYPE};
use crate::codegen::expr::translate_expr;
use crate::typ;
use bigdecimal::{BigDecimal, Zero};
use ir_lang::*;

pub fn translate_object_ctor(ctor: &typ::ast::ObjectCtor, builder: &mut Builder) -> Ref {
    let object_ty = builder.translate_type(&ctor.annotation.ty());

    let struct_id = match &object_ty {
        Type::RcPointer(VirtualTypeID::Class(struct_id)) => *struct_id,
        Type::Struct(struct_id) => *struct_id,
        _ => panic!(
            "type of object ctor expr `{}` must be a record or class",
            ctor
        ),
    };

    let struct_def = builder
        .get_struct(struct_id)
        .unwrap_or_else(|| {
            panic!("target type {} referenced in object ctor must exist", ctor.type_expr
                .as_ref()
                .map(|expr| expr.to_string())
                .unwrap_or_else(|| object_ty.to_string()))
        })
        .clone();

    // either local struct of the correct type for value types, or a rc pointer to the struct
    // type for rc class types
    let out_val = builder.local_new(object_ty.clone(), None);

    if object_ty.is_rc() {
        // allocate class struct at out pointer
        builder.rc_new(out_val.clone(), struct_id, false);
    }

    builder.scope(|builder| {
        for member in &ctor.args.members {
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

            let field_ptr = builder.local_temp(field_def.ty.clone().ptr());
            builder.append(Instruction::Field {
                out: field_ptr.clone(),
                a: out_val.clone(),
                of_ty: object_ty.clone(),
                field: field_id,
            });

            builder.mov(field_ptr.clone().to_deref(), member_val);
            builder.retain(field_ptr.to_deref(), &field_def.ty);
        }
    });

    out_val
}

pub fn translate_collection_ctor(ctor: &typ::ast::CollectionCtor, builder: &mut Builder) -> Ref {
    let ctor_ty = ctor.annotation.ty();
    match ctor_ty.as_ref() {
        typ::Type::Array(array_ty) => {
            translate_static_array_ctor(ctor, &array_ty.element_ty, array_ty.dim, builder)
        },

        typ::Type::DynArray { element } => {
            translate_dyn_array_ctor(ctor, element, builder)
        },

        typ::Type::Set(set_type) => {
            let flags_type = builder.translate_type(ctor_ty.as_ref());
            translate_set_ctor(ctor, set_type, flags_type, builder)
        }

        unimpl => unimplemented!("IR for collection constructor {} of type {}", ctor, unimpl),
    }
}

fn translate_static_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    dim: usize,
    builder: &mut Builder,
) -> Ref {
    let el_ty = builder.translate_type(element);

    let array_ty = el_ty.clone().array(dim);
    let arr = builder.local_new(array_ty.clone(), None);

    if dim > 0 {
        builder.scope(|builder| {
            let el_ptr = builder.local_temp(el_ty.clone().ptr());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    let index = i32::try_from(i).expect("invalid array index in array ctor");

                    builder.append(Instruction::Element {
                        out: el_ptr.clone(),
                        a: arr.clone(),
                        index: Value::LiteralI32(index),
                        element: el_ty.clone(),
                    });

                    let el_init = translate_expr(&el.value, builder);

                    builder.mov(el_ptr.clone().to_deref(), el_init);
                });
            }
        });
    }

    arr
}

fn translate_dyn_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    let elem_ty = builder.translate_type(element);

    // should be a class rc-ptr to the unique class for this dyn array element type
    let array_ty = builder.translate_type(&ctor.annotation.ty());
    let struct_id = match &array_ty {
        Type::RcPointer(VirtualTypeID::Class(struct_id)) => *struct_id,
        _ => unreachable!("dynamic array must have an rc class type"),
    };

    let arr = builder.local_new(array_ty.clone(), None);

    // allocate the array object itself
    builder.scope(|builder| {
        builder.rc_new(arr.clone(), struct_id, false);

        // get pointer to the length
        let len_ref = builder.local_temp(Type::I32.ptr());
        builder.append(Instruction::Field {
            out: len_ref.clone(),
            of_ty: array_ty.clone(),
            field: DYNARRAY_LEN_FIELD,
            a: arr.clone(),
        });

        // set length
        let len = i32::try_from(ctor.elements.len()).expect("invalid dynamic array ctor length");
        builder.mov(len_ref.clone().to_deref(), Value::LiteralI32(len));

        // get pointer to storage pointer
        let arr_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());
        builder.append(Instruction::Field {
            out: arr_ptr.clone(),
            of_ty: array_ty,
            field: DYNARRAY_PTR_FIELD,
            a: arr.clone(),
        });

        // allocate array storage
        if len > 0 {
            let alloc_size = builder.local_temp(Type::I32);
            builder.mul(alloc_size.clone(), Value::SizeOf(elem_ty.clone()), Value::LiteralI32(len));

            let elements_mem = builder.local_temp(Type::U8.ptr());
            builder.get_mem(alloc_size, elements_mem.clone());
            builder.cast(arr_ptr.clone().to_deref(), elements_mem, elem_ty.clone().ptr());

            let el_ptr = builder.local_temp(elem_ty.clone().ptr());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    // we know this cast is OK because we check the length is in range of i32 previously
                    let index = Value::LiteralI32(i as i32);

                    // el_ptr := arr_ptr^ + i
                    builder.add(el_ptr.clone(), arr_ptr.clone().to_deref(), index);

                    // el_ptr^ := el
                    let el = expr::translate_expr(&el.value, builder);
                    builder.mov(el_ptr.clone().to_deref(), el);

                    // retain each element. we don't do this for static arrays because retaining
                    // a static array retains all its elements - for dynamic arrays, retaining
                    // the array object itself does not retain the elements
                    builder.retain(el_ptr.clone().to_deref(), &elem_ty);
                });
            }
        } else {
            builder.mov(arr_ptr.to_deref(), Value::LiteralNull);
        }
    });

    arr
}

fn translate_set_ctor(
    ctor: &typ::ast::CollectionCtor,
    set_type: &typ::SetType,
    flags_type: Type,
    builder: &mut Builder
) -> Ref {    
    let set_result = builder.local_temp(flags_type.clone());

    // zero-init the value
    let word_ptr = builder.local_temp(WORD_TYPE.ptr());
    let zero_word = Value::from_literal_val(BigDecimal::zero(), &WORD_TYPE).unwrap();

    for word in 0..set_word_count(set_type.flags_type_bits()) {
        builder.field(word_ptr.clone(), set_result.clone(), flags_type.clone(), FieldID(word));
        builder.mov(word_ptr.clone().to_deref(), zero_word.clone());
    }
    
    let item_type = builder.translate_type(&set_type.item_type);

    let set_min = BigDecimal::from(set_type.min.as_i128());
    let min_val = Value::from_literal_val(set_min.clone(), &item_type)
        .unwrap_or_else(|| panic!("set value type must be a valid numeric primitive, was: {} ({})", set_min, item_type));
    
    let bit = builder.local_temp(Type::U8);

    for item in &ctor.elements {
        let item_val = translate_expr(&item.value, builder);
        let item_index = builder.sub_to_val(item_val, min_val.clone(), &item_type);

        builder.cast(bit.clone(), item_index, Type::U8);
        
        builder.set_include(set_result.clone(), bit.clone(), set_type);
    }

    set_result
}
