use crate::codegen::ir;
use crate::codegen::typ::TypePattern;
use crate::codegen::Builder;
use crate::typ::Specializable;

pub struct PatternMatchBinding {
    pub name: String,
    pub ty: ir::Type,
    pub binding_ref: ir::Ref,
}

impl PatternMatchBinding {
    // allocate a new local in the builder with the name and type of this binding and copy + retain
    // the binding value into it
    pub fn bind_local(&self, builder: &mut Builder) {
        builder.comment(&format!(
            "pattern binding {}: {}",
            self.name,
            builder.pretty_ty_name(&self.ty)
        ));

        let local = builder.local_new(self.ty.clone(), Some(self.name.clone()));
        builder.mov(local.clone(), self.binding_ref.clone());
        builder.retain(local, &self.ty);
    }
}

pub struct PatternMatchOutput {
    pub is_match: ir::Value,
    pub bindings: Vec<PatternMatchBinding>,
}

pub fn translate_pattern_match(
    pattern: &TypePattern,
    target_val: &ir::Ref,
    target_ty: &ir::Type,
    builder: &mut Builder
) -> PatternMatchOutput {
    match pattern {
        TypePattern::Type { binding, ty, .. } => {
            let is_ty = builder.translate_type(ty);
            let is = translate_is_ty(target_val.clone(), &target_ty, &is_ty, builder);

            let bindings = match binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();

                    // this needs to create a cast, even for static non-ref types - the binding
                    // will be of the supposed type even if the check will always fail, and the
                    // instructions that follow must be valid
                    let binding_val_ptr = builder.local_temp(ir::Type::Nothing.ptr());
                    builder.addr_of(binding_val_ptr.clone(), target_val.clone());

                    let binding_ref_ptr = builder.local_temp(is_ty.clone().ptr());
                    builder.cast(binding_ref_ptr.clone(), binding_val_ptr.clone(), is_ty.clone().ptr());

                    let binding_ref = binding_ref_ptr.to_deref();

                    vec![PatternMatchBinding {
                        name: binding_name,
                        ty: is_ty,
                        binding_ref,
                    }]
                },
                None => Vec::new(),
            };

            PatternMatchOutput {
                is_match: is,
                bindings,
            }
        },

        TypePattern::NegatedType { ty, .. } => {
            let is_not_ty = builder.translate_type(ty);
            let is = translate_is_ty(target_val.clone(), &target_ty, &is_not_ty, builder);

            let is_not = builder.not_to_val(is);

            PatternMatchOutput {
                is_match: is_not,
                bindings: Vec::new(),
            }
        },

        TypePattern::VariantCase {
             variant,
             case,
             data_binding,
             ..
         } => {
            let variant = (**variant).clone()
                .apply_type_args(builder.generic_context(), builder.generic_context());

            let (struct_id, case_index, case_ty) = builder
                .translate_variant_case(&variant, &case.name);

            let variant_ty = ir::Type::Variant(struct_id);

            let bindings = match data_binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();

                    let case_ty = case_ty
                        .cloned()
                        .expect("variant pattern with binding must refer to a case with data");

                    let data_ptr = builder.local_temp(case_ty.clone().ptr());

                    builder.append(ir::Instruction::VariantData {
                        out: data_ptr.clone(),
                        a: target_val.clone(),
                        of_ty: variant_ty.clone(),
                        tag: case_index,
                    });

                    vec![PatternMatchBinding {
                        name: binding_name,
                        binding_ref: data_ptr.to_deref(),
                        ty: case_ty,
                    }]
                },

                None => Vec::new(),
            };

            let is = translate_is_variant(target_val.clone(), variant_ty, case_index, builder);

            PatternMatchOutput {
                is_match: ir::Value::Ref(is),
                bindings,
            }
        },

        TypePattern::NegatedVariantCase { variant, case, .. } => {
            let (struct_id, case_index, _case_ty) =
                builder.translate_variant_case(variant, &case.name);

            let variant_ty = ir::Type::Variant(struct_id);
            let is = translate_is_variant(target_val.clone(), variant_ty, case_index, builder);

            let is_not = builder.not_to_val(ir::Value::Ref(is));

            PatternMatchOutput {
                is_match: is_not,
                bindings: Vec::new(),
            }
        },
    }
}

pub fn translate_is_ty(
    val: ir::Ref,
    val_ty: &ir::Type,
    ty: &ir::Type,
    builder: &mut Builder
) -> ir::Value {
    // eprintln!("is_ty pattern: {} is {}?", builder.pretty_ty_name(val_ty), builder.pretty_ty_name(ty));
    
    if let ir::Type::RcPointer(class_id) = ty {
        // casting strong or weak RC type to strong RC type: do a dynamic check
        if val_ty.is_rc() {
            let result = builder.local_temp(ir::Type::Bool);
            builder.class_is(result.clone(), val, *class_id);

            return ir::Value::Ref(result);
        }
    }

    // any other type combination must match exactly
    let same_ty = *val_ty == *ty;
    ir::Value::LiteralBool(same_ty)
}

fn translate_is_variant(
    val: ir::Ref,
    variant_ty: ir::Type,
    case_index: usize,
    builder: &mut Builder,
) -> ir::Ref {
    let tag_ptr = builder.local_temp(ir::Type::I32.ptr());
    builder.append(ir::Instruction::VariantTag {
        out: tag_ptr.clone(),
        a: val,
        of_ty: variant_ty,
    });
    
    let tag_val = ir::Value::LiteralI32(case_index as i32);

    let is = builder.local_temp(ir::Type::Bool);
    builder.eq(is.clone(), tag_ptr.to_deref(), tag_val);

    is
}
