use crate::codegen::IRBuilder;
use crate::ir;
use crate::typ;
use crate::typ::MatchPattern;
use ir::InstructionBuilder;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::ident::Ident;

pub struct PatternMatchBinding {
    pub name: Arc<String>,
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

        let local = builder.local_var(self.ty.clone(), Some(self.name.clone()));
        builder.mov(local.clone(), self.binding_ref.clone());
        builder.retain(local, self.ty.clone());
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

        MatchPattern::Name { annotation: typ::Value::Type(is_ty, _span), .. } => {
            let is_ty = builder.translate_type(is_ty);
            translate_is_ty(target_val.clone(), &target_ty, &is_ty, builder)
        },

        MatchPattern::Name { annotation: typ::Value::VariantCase(case_val), .. } => {
            let (id, case_index, _) = builder
                .translate_variant_case(&case_val.variant_name, &case_val.case.name);

            let variant_ty = id.to_variant_type();

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
    value_type: &ir::Type,
    builder: &mut IRBuilder
) -> Vec<PatternMatchBinding> {
    match pattern {
        MatchPattern::Not { .. } => {
            Vec::new()
        }

        MatchPattern::Name { annotation: typ::Value::Type(is_ty, _), .. } => {
            match binding {
                Some(binding) => {
                    let binding_type = builder.translate_type(is_ty);

                    // this needs to create a cast, even for static non-ref types - the binding
                    // will be of the supposed type even if the check will always fail, and the
                    // instructions that follow must be valid
                    // the cast must be by-ref, since casting between ref types is always valid
                    // but casting values directly may produce invalid casts when instantiating
                    // generic code
                    let value_ref = builder.local_temp(value_type.temp_ref());
                    builder.make_ref(value_ref, target_val.clone());

                    let binding_ref = builder.local_temp(binding_type.temp_ref());
                    builder.cast(binding_ref, value_ref, binding_type.temp_ref());

                    vec![PatternMatchBinding {
                        name: binding.name.clone(),
                        ty: binding_type,
                        binding_ref: binding_ref.to_deref(),
                    }]
                },

                None => {
                    Vec::new()
                },
            }
        },

        MatchPattern::Name { annotation: typ::Value::VariantCase(case_val), .. } => {
            let (type_id, case_index, case_ty) = builder
                .translate_variant_case(&case_val.variant_name, &case_val.case.name);

            let variant_ty = Rc::new(type_id).to_variant_type();

            match binding {
                Some(binding) => {
                    let case_ty = case_ty
                        .clone()
                        .expect("variant pattern with binding must refer to a case with data");

                    let data_ref = target_val.vardata_ref(variant_ty.clone(), case_index);

                    vec![PatternMatchBinding {
                        name: binding.name.clone(),
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
    value_type: &ir::Type,
    is_type: &ir::Type,
    builder: &mut IRBuilder
) -> ir::Value {
    let is_dynamic = match (value_type, is_type) {
        // if either side is of a generic type, the check must be dynamic
        (ir::Type::Generic(..), _)
        | (_, ir::Type::Generic(..)) => true,

        // if the target type is an object, and the value is a strong or weak object, the
        // check is dynamic
        (_, ir::Type::Object(..)) => value_type.is_object(),

        _ => false,
    };

    if is_dynamic {
        let result = builder.local_temp(ir::Type::Bool);
        builder.is_type(result, val, value_type.clone(), is_type.clone());

        return result.value();
    }
    
    // TODO
    // it should be possible to box a value type, and for boxes of value types to act as
    // implementations of their implemented interfaces, but for now dynamic checks of this kind
    // will always fail

    // any other type combination must match exactly
    let same_ty = *value_type == *is_type;
    ir::Value::LiteralBool(same_ty)
}

fn translate_is_variant(
    val: ir::Ref,
    variant_ty: ir::Type,
    case_index: usize,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let tag_ptr = val.vartag_ref(variant_ty);

    let tag_val = ir::Value::LiteralI32(case_index as i32);

    let is = builder.local_temp(ir::Type::Bool);
    builder.eq(is, tag_ptr.to_deref(), tag_val);

    is.to_ref()
}
