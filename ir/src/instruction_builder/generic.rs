use crate::{BinOpInstruction, FunctionSig};
use crate::FunctionDef;
use crate::Instruction;
use crate::InstructionBuilder;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::UnaryOpInstruction;
use crate::Value;
use std::collections::BTreeMap;
use std::collections::HashMap;
use terapascal_common::SharedStringKey;

type TypeMap = HashMap<SharedStringKey, Type>;
type LocalMap = BTreeMap<LocalID, LocalID>;

pub fn instantiate_generic(
    def: &FunctionDef,
    types: &TypeMap,
    builder: &mut impl InstructionBuilder,
) {
    let mut locals = LocalMap::new();

    for (instruction, source) in def.body.iter() {
        if let Some(span) = source {
            builder.push_source(span.clone());
        }

        match instruction {
            // release/retain instructions where the target ref is generic need to be expanded
            // into deep release/retain operations for the appropriate type
            Instruction::Release { at, released_out, weak } => {
                let Some(ref_type) = at.find_type(builder.local_stack(), builder.metadata()) else {
                    panic!("instantiate_generic: unable to determine type of reference: {:?}", at);
                };

                if ref_type.is_object() {
                    builder.release(at.clone(), *weak, released_out.clone());
                } else {
                    let ref_type = ref_type.into_owned();
                    builder.visit_deep(at.clone(), &ref_type, |builder, item_type, item_ref| {
                        if item_type.is_object() {
                            builder.release(item_ref, *weak, Ref::Discard);
                            true
                        } else {
                            false
                        }
                    });
                    builder.mov(released_out.clone(), false);
                }
            },
            Instruction::Retain { at, weak } => {
                let Some(ref_type) = at.find_type(builder.local_stack(), builder.metadata()) else {
                    panic!("instantiate_generic: unable to determine type of reference: {:?}", at);
                };

                if ref_type.is_object() {
                    builder.retain(at.clone(), *weak);
                } else {
                    let ref_type = ref_type.into_owned();
                    builder.visit_deep(at.clone(), &ref_type, |builder, item_type, item_ref| {
                        if item_type.is_object() {
                            builder.retain(item_ref, *weak);
                            true
                        } else {
                            false
                        }
                    });
                    builder.retain_deep(at.clone(), &ref_type);
                }
            },

            comment @ Instruction::Comment(..) => {
                builder.emit(comment.clone());
            },

            Instruction::LocalAlloc(id, ty) => {
                // remapped locals don't need any autorelease behaviour
                let remapped_id = builder.local_temp(remap_type(ty, types));
                locals.insert(*id, remapped_id);
            },

            Instruction::Move { out, new_val } => {
                builder.mov(remap_ref(out, &locals, &types), remap_val(new_val, &locals, &types));
            },

            Instruction::Add(op) => {
               builder.emit(remap_bin_op(op, &locals, &types, Instruction::Add))
            },

            Instruction::Sub(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Sub))
            },
            Instruction::Mul(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Mul))
            },
            Instruction::IDiv(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::IDiv))
            },
            Instruction::FDiv(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::FDiv))
            },
            Instruction::Mod(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Mod))
            },
            Instruction::Shl(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Shl))
            },
            Instruction::Shr(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Shr))
            },
            Instruction::BitAnd(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::BitAnd))
            },
            Instruction::BitOr(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::BitOr))
            },
            Instruction::BitXor(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::BitXor))
            },
            Instruction::BitNot(op) => {
                builder.emit(remap_unary_op(op, &locals, &types, Instruction::BitNot))
            },
            Instruction::Eq(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Eq))
            },
            Instruction::Gt(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Gt))
            },
            Instruction::Lt(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Lt))
            },
            Instruction::Lte(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Lte))
            },
            Instruction::Gte(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Gte))
            },
            Instruction::Not(op) => {
                builder.emit(remap_unary_op(op, &locals, &types, Instruction::Not))
            },
            Instruction::And(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::And))
            },
            Instruction::Or(op) => {
                builder.emit(remap_bin_op(op, &locals, &types, Instruction::Or))
            },
            Instruction::AddrOf { out, a } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_ref(a, &locals, &types);
                builder.emit(Instruction::AddrOf { out, a });
            },
            Instruction::MakeRef { out, a } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_ref(a, &locals, &types);
                builder.emit(Instruction::MakeRef { out, a });
            },
            Instruction::Length { out, a, of_type } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_ref(a, &locals, &types);
                let of_type = remap_type(of_type, &types);
                builder.emit(Instruction::Length { out, a, of_type });
            },

            Instruction::Call { out, args, function, type_args } => {
                let function = remap_val(function, &locals, &types);
                let out = out
                    .as_ref()
                    .map(|r| remap_ref(r, &locals, &types));
                let args = args
                    .iter()
                    .map(|v| remap_val(v, &locals, &types))
                    .collect();
                let type_args = type_args
                    .iter()
                    .map(|t| remap_type(t, &types))
                    .collect();

                builder.emit(Instruction::Call {
                    out,
                    args,
                    function,
                    type_args,
                })
            },

            Instruction::VirtualCall { out, iface_id, method, self_arg, rest_args } => {
                let out = out.as_ref().map(|r| remap_ref(r, &locals, &types));
                let self_arg = remap_val(self_arg, &locals, &types);

                let rest_args = rest_args.iter()
                    .map(|v| remap_val(v, &locals, &types))
                    .collect();

                builder.emit(Instruction::VirtualCall {
                    out,
                    self_arg,
                    rest_args,
                    iface_id: *iface_id,
                    method: *method,
                })
            }

            Instruction::ClassIs { out, a, class_id } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_val(a, &locals, &types);

                builder.emit(Instruction::ClassIs {
                    out,
                    a,
                    class_id: class_id.clone(),
                })
            },

            Instruction::Label(label) => {
                builder.emit(Instruction::Label(*label));
            },

            Instruction::Jump { dest } => {
                builder.emit(Instruction::Jump { dest: *dest });
            },

            Instruction::JumpIf { dest, test } => {
                builder.emit(Instruction::JumpIf {
                    dest: *dest,
                    test: remap_val(test, &locals, &types),
                });
            },

            Instruction::NewObject { out, type_id, immortal } => {
                let out = remap_ref(out, &locals, &types);
                builder.emit(Instruction::NewObject {
                    out,
                    type_id: *type_id,
                    immortal: *immortal,
                });
            },

            Instruction::NewArray { out, count, immortal, element_type } => {
                let out = remap_ref(out, &locals, &types);
                let count = remap_val(count, &locals, &types);
                let element_type = remap_type(element_type, &types);

                builder.emit(Instruction::NewArray {
                    out,
                    count,
                    element_type,
                    immortal: *immortal,
                });
            },

            Instruction::NewBox { out, value_type, immortal } => {
                let out = remap_ref(out, &locals, &types);
                let value_type = remap_type(value_type, &types);
                builder.emit(Instruction::NewBox {
                    out,
                    value_type,
                    immortal: *immortal,
                });
            },

            Instruction::Raise { val } => {
                builder.emit(Instruction::Raise {
                    val: remap_ref(val, &locals, &types)
                });
            },

            Instruction::Cast { out, a, ty } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_val(a, &locals, &types);
                let ty = remap_type(ty, &types);

                builder.emit(Instruction::Cast {
                    out,
                    a,
                    ty,
                });
            },
        }

        if source.is_some() {
            builder.pop_source();
        }
    }
}

pub fn instantiate_sig(generic_sig: &FunctionSig, types: &TypeMap) -> FunctionSig {
    let param_tys = generic_sig.param_tys
        .iter()
        .map(|t| remap_type(t, types))
        .collect();

    FunctionSig {
        param_tys,
        return_ty: remap_type(&generic_sig.return_ty, types),
    }
}

fn remap_bin_op(
    op: &BinOpInstruction,
    locals: &LocalMap,
    types: &TypeMap,
    instruction_ctor: fn(BinOpInstruction) -> Instruction
) -> Instruction {
    let out = remap_ref(&op.out, locals, types);
    let a = remap_val(&op.a, locals, types);
    let b = remap_val(&op.b, locals, types);

    instruction_ctor(BinOpInstruction { out, a, b })
}

fn remap_unary_op(
    op: &UnaryOpInstruction,
    locals: &LocalMap,
    types: &TypeMap,
    instruction_ctor: fn(UnaryOpInstruction) -> Instruction
) -> Instruction {
    let out = remap_ref(&op.out, locals, types);
    let a = remap_val(&op.a, locals, types);

    instruction_ctor(UnaryOpInstruction { out, a })
}

fn remap_ref(r: &Ref, locals: &LocalMap, types: &TypeMap) -> Ref {
    match r {
        Ref::Local(id) => {
            let mapped_id = locals
                .get(id)
                .unwrap_or_else(|| panic!("missing local in local map: {id}"));
            Ref::Local(*mapped_id)
        },
        Ref::Deref(val) => remap_val(val, locals, types).deref(),
        Ref::Field(field_ref) => {
            let instance = remap_ref(&field_ref.instance, locals, types);
            instance.field_ref(remap_type(&field_ref.instance_type, types), field_ref.field)
        },
        Ref::Element(element_ref) => {
            let index = remap_val(&element_ref.index, locals, types);
            let instance = remap_ref(&element_ref.instance, locals, types);
            instance.element_ref(remap_type(&element_ref.instance_type, types), index)
        },
        Ref::VariantData(data_ref) => {
            let instance = remap_ref(&data_ref.instance, locals, types);
            instance.vardata_ref(
                remap_type(&data_ref.instance_type, types),
                data_ref.case_index,
            )
        },
        Ref::VariantTag(tag_ref) => {
            let instance = remap_ref(&tag_ref.instance, locals, types);
            instance.vartag_ref(remap_type(&tag_ref.instance_type, types))
        },
        _ => r.clone(),
    }
}

fn remap_val(v: &Value, locals: &LocalMap, types: &TypeMap) -> Value {
    match v {
        Value::Ref(r) => remap_ref(r, locals, types).value(),
        Value::Default(t) => Value::Default(remap_type(t, types)),
        _ => v.clone(),
    }
}

fn remap_type(t: &Type, types: &TypeMap) -> Type {
    match t {
        Type::Generic(name) => {
            let real_type = types
                .get(name.as_str())
                .unwrap_or_else(|| panic!("missing generic type in type map: {name}"));
            real_type.clone()
        },

        _ => t.clone(),
    }
}
