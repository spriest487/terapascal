use crate::ast::Ident;
use crate::codegen::ir;
use crate::codegen::IRBuilder;
use crate::typ;
use crate::typ::MatchPattern;
use crate::typ::Specializable;
use std::sync::Arc;
use terapascal_ir::instruction_builder::InstructionBuilder;

pub struct PatternMatchBinding {
    pub name: String,
    pub ty: ir::Type,
    pub binding_ref: ir::Ref,
}

impl PatternMatchBinding {
    // allocate a new local in the builder with the name and type of this binding and copy + retain
    // the binding value into it
    pub fn bind_local(&self, builder: &mut IRBuilder) {
        builder.comment(&format!(
            "pattern binding {}: {}",
            self.name,
            builder.pretty_ty_name(&self.ty)
        ));

        let local = builder.local_new(self.ty.clone(), Some(Arc::new(self.name.clone()))).to_ref();
        builder.mov(local.clone(), self.binding_ref.clone());
        builder.retain_deep(local, &self.ty);
    }
}

pub fn translate_pattern_match_is(
    pattern: &MatchPattern,
    binding: Option<&Ident>,
    target_val: &ir::Ref,
    target_ty: &ir::Type,
    builder: &mut IRBuilder
) -> ir::Value {
    match pattern {
        MatchPattern::Not { pattern, .. } => {
            assert!(binding.is_none(), "negated binding ({}) should never have a binding", pattern);

            let inner_pattern = translate_pattern_match_is(pattern, None, target_val, target_ty, builder);
            builder.not_to_val(inner_pattern)
        }
        
        MatchPattern::Name { annotation: typ::Value::Type(is_ty, _), .. } => {
            let is_ty = builder.translate_type(is_ty);
            translate_is_ty(target_val.clone(), &target_ty, &is_ty, builder)
        },

        MatchPattern::Name { annotation: typ::Value::VariantCase(case_val), .. } => {
            let variant = case_val.variant_name.as_ref().clone()
                .apply_type_args(builder.generic_context(), builder.generic_context());

            let (struct_id, case_index, _) = builder
                .translate_variant_case(&variant, &case_val.case.name);

            let variant_ty = ir::Type::Variant(struct_id);

            let is = translate_is_variant(target_val.clone(), variant_ty, case_index, builder);

            ir::Value::Ref(is)
        },
        
        MatchPattern::Name { annotation: illegal, .. } => {
            panic!("illegal value in typechecked pattern item {pattern}: {illegal}")
        }
    }
}

pub fn translate_pattern_match_bindings(
    pattern: &MatchPattern,
    binding: Option<&Ident>,
    target_val: &ir::Ref,
    builder: &mut IRBuilder
) -> Vec<PatternMatchBinding> {
    match pattern {
        MatchPattern::Not { .. } => {
            Vec::new()
        }

        MatchPattern::Name { annotation: typ::Value::Type(is_ty, _), .. } => {
            match binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();
                    let binding_type = builder.translate_type(is_ty);

                    // this needs to create a cast, even for static non-ref types - the binding
                    // will be of the supposed type even if the check will always fail, and the
                    // instructions that follow must be valid
                    let bound_val = builder.local_temp(binding_type.clone());
                    builder.cast(bound_val, target_val.clone(), binding_type.clone());

                    vec![PatternMatchBinding {
                        name: binding_name,
                        ty: binding_type,
                        binding_ref: bound_val.to_ref(),
                    }]
                },

                None => {
                    Vec::new()
                },
            }
        },

        MatchPattern::Name { annotation: typ::Value::VariantCase(case_val), .. } => {
            let variant = case_val.variant_name.as_ref().clone()
                .apply_type_args(builder.generic_context(), builder.generic_context());

            let (struct_id, case_index, case_ty) = builder
                .translate_variant_case(&variant, &case_val.case.name);

            let variant_ty = ir::Type::Variant(struct_id);

            match binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();

                    let case_ty = case_ty
                        .cloned()
                        .expect("variant pattern with binding must refer to a case with data");

                    let data_ref = builder.local_temp(case_ty.clone().temp_ref());
                    builder.vardata(data_ref, target_val.clone(), variant_ty.clone(), case_index);

                    vec![PatternMatchBinding {
                        name: binding_name,
                        binding_ref: data_ref.to_deref(),
                        ty: case_ty,
                    }]
                },

                None => {
                    Vec::new()
                },
            }
        },

        MatchPattern::Name { annotation: illegal, .. } => {
            panic!("illegal value in typechecked pattern item {pattern}: {illegal}")
        }
    }
}

pub fn translate_is_ty(
    val: ir::Ref,
    val_ty: &ir::Type,
    ty: &ir::Type,
    builder: &mut IRBuilder
) -> ir::Value {
    // eprintln!("is_ty pattern: {} is {}?", builder.pretty_ty_name(val_ty), builder.pretty_ty_name(ty));
    
    if let ir::Type::Object(class_id) = ty {
        // casting strong or weak RC type to strong RC type: do a dynamic check
        if val_ty.is_rc() {
            let result = builder.local_temp(ir::Type::Bool);
            builder.class_is(result, val, class_id.clone());

            return result.value();
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
    builder: &mut IRBuilder,
) -> ir::Ref {
    let tag_ptr = builder.local_temp(ir::Type::I32.temp_ref());
    builder.vartag(tag_ptr, val, variant_ty);
    
    let tag_val = ir::Value::LiteralI32(case_index as i32);

    let is = builder.local_temp(ir::Type::Bool);
    builder.eq(is, tag_ptr.to_deref(), tag_val);

    is.to_ref()
}
