use crate::ArgID;
use crate::BinOpInstruction;
use crate::FunctionDef;
use crate::FunctionIdentity;
use crate::FunctionParamInfo;
use crate::FunctionRef;
use crate::FunctionSig;
use crate::GlobalRef;
use crate::Instruction;
use crate::InstructionBuilder;
use crate::InterfaceDef;
use crate::LocalID;
use crate::MetadataSource;
use crate::InterfaceMethod;
use crate::ObjectID;
use crate::Ref;
use crate::StructDef;
use crate::StructFieldDef;
use crate::Type;
use crate::TypeDefID;
use crate::TypeParam;
use crate::TypeRef;
use crate::UnaryOpInstruction;
use crate::Value;
use crate::VariantCase;
use crate::VariantDef;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::SharedStringKey;

type TypeMap = HashMap<SharedStringKey, Type>;
type LocalMap = BTreeMap<LocalID, LocalID>;

/// Calculates the list of type params required to call a function with the given identity.
/// The argument list passed to an invocation of a method belonging to a generic type is expected
/// to provide both the type's arguments and the method's type arguments (if any) consecutively
/// in the same list of args, in that order
pub fn invocation_type_params<'a>(
    identity: &'a FunctionIdentity,
    metadata: &'a impl MetadataSource,
) -> impl Iterator<Item=&'a TypeParam> {
    let enclosing_type_params = match identity {
        FunctionIdentity::Method { declaring_type, .. } => {
            type_def_type_params(declaring_type, metadata)
        }

        _ => {
            &[]
        }
    };

    let func_type_params = match identity {
        FunctionIdentity::Global(func_name) => {
            func_name.type_params.as_slice()
        }

        FunctionIdentity::Method { type_params, .. } => {
            type_params.as_slice()
        }

        FunctionIdentity::Internal { type_params, .. } => {
            type_params.as_slice()
        }

        _ => {
            &[]
        },
    };

    enclosing_type_params
        .iter()
        .chain(func_type_params.iter())
}

// todo: should return borrowed refs once types actually contain type params
fn type_def_type_params<'a>(def_type: &'a Type, metadata: &'a impl MetadataSource) -> &'a [TypeParam] {
    match def_type {
        Type::Struct(struct_ref) => {
            type_params_from_struct(struct_ref.def_id, metadata)
        }

        Type::Variant(variant_ref) => {
            let Some(variant_def) = metadata.get_variant_def(variant_ref.def_id) else {
                panic!("build_invocation_type_map: missing def for variant {}", variant_ref.def_id.0);
            };

            &variant_def.name.type_params
        }

        Type::Object(object_id) | Type::WeakObject(object_id) => {
            match object_id {
                ObjectID::Class(class_ref) => {
                    type_params_from_struct(class_ref.def_id, metadata)
                }
                ObjectID::Interface(iface_ref) => {
                    let Some(iface_def) = metadata.get_interface_def(iface_ref.def_id) else {
                        panic!("build_invocation_type_map: missing def for interface {}", iface_ref.def_id.0);
                    };

                    &iface_def.name.type_params
                }

                _ => &[],
            }
        }

        _ => &[],
    }
}

pub fn build_type_map<'a>(
    params: impl IntoIterator<Item=&'a TypeParam>,
    args: &[Type],
    types: &mut TypeMap,
) {
    for (position, type_param) in params.into_iter().enumerate() {
        let arg_type = &args[position];

        let param_name = type_param.name.clone();

        if let Some(prev) = types.insert(SharedStringKey(param_name), arg_type.clone()) {
            assert_eq!(prev, *arg_type, "build_type_map: inconsistent arg types");
        }
    }
}

fn type_params_from_struct(id: TypeDefID, metadata: &impl MetadataSource) -> &[TypeParam] {
    let Some(def) = metadata.get_struct_def(id) else {
        panic!("build_invocation_type_map: missing def for struct {}", id.0);
    };

    let Some(name) = def.identity.name() else {
        return &[];
    };

    &name.type_params
}

// TODO: errors
pub fn instantiate_function_def(
    def: &FunctionDef,
    type_params: &[TypeParam],
    types: &TypeMap,
    builder: &mut impl InstructionBuilder,
) {
    assert!(type_params.iter().all(|param| types.contains_key(&param.name)));

    let mut locals = LocalMap::new();

    let result_type = instantiate_type(&def.sig.result_type, types);
    if result_type != Type::Nothing {
        builder.local_stack_mut().bind_result(def.sig.result_type.clone());
    }

    for (i, param_type) in def.sig.param_types.iter().enumerate() {
        let param_type = instantiate_type(param_type, types);
        builder.local_stack_mut().bind_unnamed_param(ArgID(i), param_type, false);
    }

    for (instruction, source) in def.body.iter() {
        if let Some(span) = source {
            builder.push_source(span.clone());
        }

        match instruction {
            Instruction::Release { at, released_out, value_type } => {
                let value_type = instantiate_type(value_type, types);
                let at = remap_ref(at, &locals, types);
                let released_out = remap_ref(released_out, &locals, types);

                builder.release(at, value_type, released_out);
            },

            Instruction::Retain { at, value_type } => {
                let value_type = instantiate_type(value_type, types);
                let at = remap_ref(at, &locals, types);

                builder.retain(at, value_type);
            },

            comment @ Instruction::Comment(..) => {
                builder.emit(comment.clone());
            },

            Instruction::LocalAlloc(id, ty) => {
                // remapped locals don't need any autorelease behaviour
                let remapped_id = builder.local_temp(instantiate_type(ty, types));
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
                let of_type = instantiate_type(of_type, &types);
                builder.emit(Instruction::Length { out, a, of_type });
            },

            Instruction::Call { out, args, function } => {
                let function = remap_val(function, &locals, &types);
                let out = out
                    .as_ref()
                    .map(|r| remap_ref(r, &locals, &types));
                let args = args
                    .iter()
                    .map(|v| remap_val(v, &locals, &types))
                    .collect();

                builder.emit(Instruction::Call {
                    out,
                    args,
                    function,
                })
            },

            Instruction::VirtualCall { out, iface_ref, method, self_arg, rest_args } => {
                let out = out.as_ref().map(|r| remap_ref(r, &locals, &types));
                let self_arg = remap_ref(self_arg, &locals, &types);

                let rest_args = rest_args.iter()
                    .map(|v| remap_val(v, &locals, &types))
                    .collect();

                builder.emit(Instruction::VirtualCall {
                    out,
                    self_arg,
                    rest_args,
                    iface_ref: iface_ref.clone(),
                    method: *method,
                })
            }

            Instruction::IsType { out, a, value_type, is_type } => {
                let out = remap_ref(out, &locals, &types);
                let a = remap_val(a, &locals, &types);

                let value_type = instantiate_type(value_type, types);
                let is_type = instantiate_type(is_type, types);

                builder.emit(Instruction::IsType {
                    out,
                    a,
                    value_type,
                    is_type,
                })
            }

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

            Instruction::NewObject { out, type_id, type_args, immortal } => {
                let out = remap_ref(out, &locals, &types);
                let type_args = type_args
                    .iter()
                    .map(|t| instantiate_type(t, &types))
                    .collect();

                builder.emit(Instruction::NewObject {
                    out,
                    type_args,
                    type_id: *type_id,
                    immortal: *immortal,
                });
            },

            Instruction::NewArray { out, count, immortal, element_type } => {
                let out = remap_ref(out, &locals, &types);
                let count = remap_val(count, &locals, &types);
                let element_type = instantiate_type(element_type, &types);

                builder.emit(Instruction::NewArray {
                    out,
                    count,
                    element_type,
                    immortal: *immortal,
                });
            },

            Instruction::NewBox { out, value_type, immortal } => {
                let out = remap_ref(out, &locals, &types);
                let value_type = instantiate_type(value_type, &types);
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
                let ty = instantiate_type(ty, &types);

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

pub fn instantiate_sig(sig: &FunctionSig, types: &TypeMap) -> FunctionSig {
    let result_type = instantiate_type(&sig.result_type, types);
    let param_types = sig.param_types
        .iter()
        .map(|ty| instantiate_type(ty, types));

    FunctionSig::new(param_types, result_type)
}

fn type_param_from_generic_arg(arg_type: &Type) -> TypeParam {
    let Type::Generic(param_name) = arg_type else {
        panic!("type_param_from_generic_arg: generic name must only contain named parameter types");
    };

    TypeParam {
        name: Arc::new(param_name.to_string()),
        constraint: None,
    }
}

pub fn instantiate_name_path(name_params: &mut [Type], type_args: &[Type], types: &mut TypeMap) {
    assert_eq!(type_args.len(), name_params.len(), "instantiate_name_path: type arg count mismatch");

    let type_params: Vec<_> = name_params
        .iter()
        .map(type_param_from_generic_arg)
        .collect();

    build_type_map(type_params.iter(), type_args, types)
}

pub fn instantiate_struct_def<'a>(
    generic_struct: &'a StructDef,
    type_args: &[Type],
) -> Cow<'a, StructDef> {
    if !generic_struct.is_generic() {
        assert_eq!(0, type_args.len(), "instantiate_struct_def: type is not generic but {} type args were provided", type_args.len());
        return Cow::Borrowed(generic_struct);
    }

    let mut types = HashMap::new();

    let name_params = match generic_struct.identity.name() {
        Some(name) => name.type_params.as_slice(),
        None => &[],
    };
    build_type_map(name_params, type_args, &mut types);

    let mut fields = BTreeMap::new();
    for (field_id, field_def) in &generic_struct.fields {
        fields.insert(*field_id, StructFieldDef {
            ty: instantiate_type(&field_def.ty, &types),
            name: field_def.name.clone(),
        });
    }

    Cow::Owned(StructDef {
        fields,
        access: generic_struct.access,
        tags: generic_struct.tags.clone(),
        identity: generic_struct.identity.clone(),
        layout: generic_struct.layout,
    })
}

pub fn instantiate_variant_def<'a>(
    generic_variant: &'a VariantDef,
    type_args: &[Type],
) -> Cow<'a, VariantDef> {
    if !generic_variant.is_generic() {
        assert_eq!(0, type_args.len(), "instantiate_variant_def: type is not generic");
        return Cow::Borrowed(generic_variant);
    }

    let mut types = HashMap::new();
    build_type_map(&generic_variant.name.type_params, type_args, &mut types);

    let tag_type = instantiate_type(&generic_variant.tag_type, &types);

    let mut cases = Vec::new();
    for case_def in &generic_variant.cases {
        let data_type = case_def.ty
            .as_ref()
            .map(|t| instantiate_type(t, &types));

        cases.push(VariantCase {
            tag: case_def.tag.clone(),
            name: case_def.name.clone(),
            ty: data_type,
        });
    }

    Cow::Owned(VariantDef {
        name: generic_variant.name.clone(),
        access: generic_variant.access,
        cases,
        tags: generic_variant.tags.clone(),
        tag_type,
    })
}

pub fn instantiate_interface_def<'a>(
    generic_def: &'a InterfaceDef,
    type_args: &[Type],
) -> Cow<'a, InterfaceDef> {
    if generic_def.name.type_params.is_empty() {
        assert_eq!(0, type_args.len(), "instantiate_interface_def: type is not generic");
        return Cow::Borrowed(generic_def);
    }

    let mut types = TypeMap::new();

    build_type_map(&generic_def.name.type_params, type_args, &mut types);

    let mut methods = Vec::with_capacity(generic_def.methods.len());

    for def_method in &generic_def.methods {
        let mut params = Vec::with_capacity(def_method.params.len());
        for def_param in &def_method.params {
            let param_type = instantiate_type(&def_param.param_type, &mut types);

            params.push(FunctionParamInfo {
                param_type,
                name: def_param.name.clone(),
                tags: def_param.tags.clone(),
            });
        }

        methods.push(InterfaceMethod {
            name: def_method.name.clone(),
            result_type: instantiate_type(&def_method.result_type, &mut types),
            params,
        });
    }

    Cow::Owned(InterfaceDef {
        name: generic_def.name.clone(),
        access: generic_def.access,
        tags: generic_def.tags.clone(),
        methods,
    })
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
            instance.field_ref(instantiate_type(&field_ref.instance_type, types), field_ref.field)
        },
        Ref::Element(element_ref) => {
            let index = remap_val(&element_ref.index, locals, types);
            let instance = remap_ref(&element_ref.instance, locals, types);
            instance.element_ref(instantiate_type(&element_ref.instance_type, types), index)
        },
        Ref::VariantData(data_ref) => {
            let instance = remap_ref(&data_ref.instance, locals, types);
            instance.vardata_ref(
                instantiate_type(&data_ref.instance_type, types),
                data_ref.case_index,
            )
        },
        Ref::VariantTag(tag_ref) => {
            let instance = remap_ref(&tag_ref.instance, locals, types);
            instance.vartag_ref(instantiate_type(&tag_ref.instance_type, types))
        },
        Ref::Global(GlobalRef::StaticTypeInfo(ty)) => {
            let ty = instantiate_type(ty, types);
            Ref::from(GlobalRef::StaticTypeInfo(Rc::new(ty)))
        }
        Ref::Global(GlobalRef::Function(func_ref)) => {
            let mut type_args = Vec::with_capacity(func_ref.args.len());
            for arg in &func_ref.args {
                type_args.push(instantiate_type(arg, types));
            }
            Ref::from(FunctionRef::new(func_ref.def_id).with_args(type_args))
        }
        _ => r.clone(),
    }
}

fn remap_val(v: &Value, locals: &LocalMap, types: &TypeMap) -> Value {
    match v {
        Value::Ref(r) => remap_ref(r, locals, types).value(),
        Value::Default(t) => Value::Default(instantiate_type(t, types)),
        _ => v.clone(),
    }
}

pub fn instantiate_type_param(p: &TypeParam, types: &TypeMap) -> TypeParam {
    let constraint = p.constraint
        .as_ref()
        .map(|t| instantiate_type(t, types));

    TypeParam {
        name: p.name.clone(),
        constraint,
    }
}

pub fn instantiate_type(t: &Type, types: &TypeMap) -> Type {
    match t {
        Type::Generic(name) => {
            let real_type = types
                .get(name.as_str())
                .unwrap_or_else(|| panic!("missing generic type in type map: {name}"));
            real_type.clone()
        },

        Type::Struct(id) => {
            let args = id.args.iter()
                .map(|t| instantiate_type(t, types));

            TypeRef::new(id.def_id, args).to_struct_type()
        }

        Type::Variant(id) => {
            let args = id.args.iter()
                .map(|t| instantiate_type(t, types));

            TypeRef::new(id.def_id, args).to_variant_type()
        }

        Type::Object(object_id) | Type::WeakObject(object_id) => {
            let object_id = match object_id {
                ObjectID::Class(id) => {
                    let args = id.args.iter()
                        .map(|t| instantiate_type(t, types));
                    let id = TypeRef::new(id.def_id, args);
                    ObjectID::Class(id)
                }

                ObjectID::Array(element) => {
                    ObjectID::Array(Rc::new(instantiate_type(element, types)))
                }

                ObjectID::Box(value) => {
                    ObjectID::Box(Rc::new(instantiate_type(value, types)))
                }

                ObjectID::AnyClosure(sig) => {
                    instantiate_sig(sig, types).into_closure_id()
                }

                other => other.clone(),
            };

            if t.is_weak() {
                object_id.to_weak_object_type()
            } else {
                object_id.to_object_type()
            }
        }

        Type::Pointer(deref_ty) => {
            instantiate_type(deref_ty, types).ptr()
        }

        Type::TempRef(deref_ty) => {
            instantiate_type(deref_ty, types).temp_ref()
        }

        Type::Array { element, dim } => {
            instantiate_type(element, types).array(*dim)
        }

        Type::Function(sig) => {
            instantiate_sig(sig, types).into_function_type()
        }

        _ => t.clone(),
    }
}
