use crate::instruction_builder::InstructionBuilder;
use crate::TypeRef;
use crate::MetadataSource;
use crate::Ref;
use std::rc::Rc;

pub fn gen_class_object_dtor_body<B>(
    builder: &mut B,
    class_id: &Rc<TypeRef>,
    object_ref: impl Into<Ref>,
) -> bool
where
    B: InstructionBuilder + ?Sized
{
    let object_ref = object_ref.into();

    let mut has_dtor_method = false;

    if let Some(dtor_func_id) = builder
        .metadata()
        .get_dtor_method(&class_id.to_class_object_type())
    {
        let dtor_ref = crate::FunctionRef::new(dtor_func_id)
            .with_args(class_id.args.clone());

        builder.call(dtor_ref, [object_ref.value()], None);

        has_dtor_method = true;
    }

    // we have to do this loop manually, because the "self" reference in a class method is
    // meant to be immutable (it's illegal to reference it even via an immutable reference in CIL).
    // using visit_deep on self would use references!
    let class_struct_def = builder
        .metadata()
        .instantiate_struct_def(class_id.def_id, &class_id.args)
        .unwrap_or_else(|| {
            let class_ty = class_id.to_class_object_type();
            let class_pretty_name = builder.metadata().pretty_type_name(&class_ty);

            panic!("gen_class_object_dtor_body: missing definition for resource struct of {class_pretty_name}", )
        })
        .into_owned();

    let class_ty = class_id.to_class_object_type();

    let mut released_any = false;

    for (field_id, field_def) in &class_struct_def.fields {
        if !field_def.ty.contains_any_object_refs(builder.metadata()) {
            continue;
        }

        let field_ref = object_ref.field_ref(class_ty.clone(), *field_id);
        builder.release(field_ref.to_deref(), field_def.ty.clone());

        released_any = true;
    }
    
    released_any || has_dtor_method
}