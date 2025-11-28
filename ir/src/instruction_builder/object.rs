use crate::instruction_builder::InstructionBuilder;
use crate::LocalID;
use crate::TypeDefID;

pub fn gen_class_object_dtor_body<B>(builder: &mut B, class_id: TypeDefID, self_param: LocalID) -> bool
where
    B: InstructionBuilder + ?Sized
{
    let class_ty = class_id.to_class_ptr_type();

    let class_pretty_name = builder.metadata().pretty_ty_name(&class_ty).into_owned();

    // we have to do this loop manually, because the "self" reference in a class method is
    // meant to be immutable (it's illegal to reference it even via an immutable reference in CIL).
    // using visit_deep on self would use references!
    let class_struct_def = builder.metadata()
        .get_struct_def(class_id)
        .unwrap_or_else(|| {
            panic!("gen_class_object_dtor_body: missing definition for resource struct of {class_pretty_name}", )
        })
        .clone();
    
    let mut released_any = false;

    for (field_id, field_def) in class_struct_def.fields {
        if !field_def.ty.contains_any_object_refs(builder.metadata()) {
            continue;
        }

        let field_ref = builder.local_temp(field_def.ty.clone().temp_ref());
        builder.field(field_ref, self_param, class_ty.clone(), field_id);
        released_any |= builder.release_deep(field_ref.to_deref(), &field_def.ty);

        assert!(released_any, "if contains_any_object_refs returns true, release_deep should find a reference to release")
    }
    
    released_any
}
