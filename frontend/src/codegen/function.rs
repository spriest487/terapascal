use crate::ast;
use crate::codegen::builder::jmp_exists;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_block;
use crate::codegen::translate_literal;
use crate::codegen::typ;
use crate::codegen::Builder;
use crate::codegen::ClosureInstance;
use crate::ir::*;
use std::iter;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: Arc<typ::FunctionSig>,
}

fn create_function_body_builder<'m>(
    lib: &mut LibraryBuilder,
    generic_ctx: typ::GenericContext,
    debug_name: Option<String>,
) -> Builder {
    let debug_comment = debug_name.map(|name| {
        let mut comment = format!("function def body of {}", name);
        if !generic_ctx.is_empty() {
            comment += format!(" with generic context {}", generic_ctx).as_str();
        }
        
        comment
    });

    let mut builder = Builder::new(lib)
        .with_generic_ctx(generic_ctx);
    
    if let Some(comment) = debug_comment {
        builder.comment(&comment);
    }

    builder
}

pub fn build_func_def(
    module: &mut LibraryBuilder,
    generic_ctx: typ::GenericContext,
    def_params: &[typ::ast::FunctionParam],
    def_return_ty: &typ::Type,
    def_locals: &[typ::ast::FunctionLocalBinding],
    def_body: &typ::ast::Block,
    is_instance_method: bool,
    debug_name: Option<String>,
) -> FunctionDef {
    let mut body_builder = create_function_body_builder(
        module,
        generic_ctx,
        debug_name.clone()
    );

    let return_ty = bind_function_return(def_return_ty, &mut body_builder);

    let def_params: Vec<_> = def_params
        .iter()
        .map(|param| FunctionParam::from_ast(param, &mut body_builder))
        .collect();
    let bound_params = bind_function_params(def_params, is_instance_method, &mut body_builder);

    init_function_locals(def_locals, &mut body_builder);

    let body = build_func_body(def_body, &return_ty, body_builder);

    FunctionDef {
        body,
        sig: FunctionSig {
            param_tys: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            return_ty,
        },
        debug_name,
    }
}

pub fn build_func_static_closure_def(
    library: &mut LibraryBuilder,
    target_func: &FunctionInstance,
    target_ir_func: &Function,
) -> FunctionDef {
    let generic_ctx = typ::GenericContext::empty();

    let params = target_func
        .sig
        .params
        .iter()
        .enumerate()
        .map(|(index, sig_param)| {
            FunctionParam {
                by_ref: matches!(sig_param.modifier, Some(ast::FunctionParamMod::Var | ast::FunctionParamMod::Out)),
                ty: library.translate_type(&sig_param.ty, &generic_ctx),
                name: format!("P{index}"),
            }
        })
        .collect::<Vec<_>>();

    let debug_name = match target_ir_func.debug_name() {
        Some(func_name) if library.opts().debug => {
            Some(format!("static closure def ({func_name})"))
        },

        _ => None,
    };

    let mut body_builder = create_function_body_builder(library, generic_ctx, debug_name.clone());

    let return_ty = bind_function_return(&target_func.sig.result_ty, &mut body_builder);

    let closure_ptr_local_id = body_builder.bind_closure_ptr();

    // this method needs to be compatible with the type-erased function pointer stored in a
    // closure struct, which has the sig "(Pointer, ...actual params)"
    let mut bound_params = bind_function_params(params, false, &mut body_builder);
    bound_params.insert(0, (closure_ptr_local_id, Type::Nothing.ptr()));

    let func_global = Ref::Global(GlobalRef::Function(target_func.id));
    let func_args = bound_params
        .iter()
        .skip(1)
        .map(|(local_id, _)| Value::Ref(Ref::Local(*local_id)))
        .collect::<Vec<_>>();

    let return_ref = match return_ty {
        Type::Nothing => None,
        _ => Some(RETURN_REF),
    };

    body_builder.call(func_global, func_args, return_ref);

    FunctionDef {
        sig: FunctionSig {
            param_tys: bound_params
                .into_iter()
                .map(|(_, param_ty)| param_ty)
                .collect(),
            return_ty,
        },
        debug_name,
        body: body_builder.finish(),
    }
}

pub fn build_closure_function_def(
    lib: &mut LibraryBuilder,
    func_def: &typ::ast::AnonymousFunctionDef,
    closure_id: TypeDefID,
    debug_name: Option<String>,
) -> FunctionDef {
    let closure_def = lib.metadata().get_struct_def(closure_id).cloned().unwrap();

    let generic_ctx = typ::GenericContext::empty();
    let mut body_builder = create_function_body_builder(lib, generic_ctx, debug_name.clone());

    let return_ty = bind_function_return(&func_def.result_ty, &mut body_builder);

    // the type-erased pointer to the closure struct is included as the 0th param but
    // *not* bound like a normal param since it can't be named from code, so bind it in the scope
    // of this function body now
    let closure_ptr_param_local_id = body_builder.bind_closure_ptr();
    let closure_ptr_param_ref = Ref::Local(closure_ptr_param_local_id);

    let def_params: Vec<_> = func_def.params
        .iter()
        .map(|param| FunctionParam::from_ast(param, &mut body_builder))
        .collect();

    let bound_params = bind_function_params(def_params, false, &mut body_builder);

    // cast the closure pointer param from the erased pointer passed in to its actual internal type
    let closure_struct_ty = Type::Struct(closure_id);
    let closure_struct_ptr_ty = closure_struct_ty.clone().ptr();
    let closure_ptr_ref = body_builder.local_temp(closure_struct_ptr_ty.clone());
    body_builder.cast(
        closure_ptr_ref.clone(),
        closure_ptr_param_ref,
        closure_struct_ptr_ty,
    );

    // copy closure members into the body scope
    // the order doesn't need to match the closure struct field order (although it probably
    // will), we just need to ensure all captures are bound to unique locals before we
    // start letting the body code allocate its own locals
    for (field_id, field_def) in closure_def.fields.iter() {
        if *field_id == CLOSURE_PTR_FIELD {
            continue;
        }

        let field_name = match field_def.name.as_ref() {
            None => continue,
            Some(name) => name,
        };

        let capture_val_ptr_ty = field_def.ty.clone().ptr();
        let capture_val_ptr_ref =
            body_builder.local_closure_capture(capture_val_ptr_ty, field_name.clone());

        body_builder.field(
            capture_val_ptr_ref.clone(),
            closure_ptr_ref.clone().to_deref(),
            closure_struct_ty.clone(),
            *field_id,
        );
    }

    let body = build_func_body(&func_def.body, &return_ty, body_builder);

    // the 0th parameter of the function is always a type-erased pointer, which we must
    // cast to the actual closure struct type in the body
    let actual_params = iter::once(Type::Nothing.ptr())
        .chain(bound_params.into_iter().map(|(_, param_ty)| param_ty))
        .collect();

    FunctionDef {
        body,
        sig: FunctionSig {
            param_tys: actual_params,
            return_ty,
        },
        debug_name,
    }
}

fn bind_function_return(return_ty: &typ::Type, builder: &mut Builder) -> Type {
    match return_ty {
        typ::Type::Nothing => Type::Nothing,
        
        return_ty => {
            let return_ty = builder.translate_type(return_ty);

            // anonymous return binding at %0
            builder.comment(&format!(
                "{} = {} (return slot)",
                LocalID(0),
                builder.pretty_ty_name(&return_ty),
            ));

            builder.bind_return();
            return_ty
        },
    }
}

#[derive(Debug, Clone)]
struct FunctionParam {
    pub name: String,
    pub ty: Type,
    pub by_ref: bool,
}

impl FunctionParam {
    fn from_ast(param: &typ::ast::FunctionParam, builder: &mut Builder) -> Self {
        let (param_ty, by_ref) = match param.get_modifier() {
            Some(ast::FunctionParamMod::Var) | Some(ast::FunctionParamMod::Out) => {
                (builder.translate_type(&param.ty).ptr(), true)
            },

            None => (builder.translate_type(&param.ty), false),
        };

        let name = param.name.to_string();

        FunctionParam {
            name,
            by_ref,
            ty: param_ty,
        }
    }
}

fn bind_function_params(
    params: impl IntoIterator<Item=FunctionParam>,
    is_instance_method: bool,
    builder: &mut Builder,
) -> Vec<(LocalID, Type)> {
    let mut bound_params = Vec::new();

    let mut is_self = is_instance_method;

    for param in params.into_iter() {
        let mut by_ref = param.by_ref;
        let mut param_ty = param.ty.clone();

        // pass the self parameter as a pointer for non-reference types
        if is_self {
            if !param.ty.is_rc() {
                assert!(!by_ref, "self param should not already be by-ref");
                by_ref = true;
                param_ty = param_ty.ptr();
            }
            is_self = false;
        }
        
        let id = builder.next_local_id();

        builder.comment(&format!("{} = {}", id, builder.pretty_ty_name(&param.ty)));
        builder.bind_param(id, param_ty.clone(), &param.name, by_ref);

        bound_params.push((id, param_ty));
    }

    for (id, ty) in &bound_params {
        builder.retain(Ref::Local(*id), ty);
    }

    bound_params
}

fn init_function_locals(locals: &[typ::ast::FunctionLocalBinding], builder: &mut Builder) {
    for local in locals {
        if local.kind == ast::BindingDeclKind::Var {
            let ty = builder.translate_type(&local.ty);

            let local_ref = builder.local_new(ty, Some(local.ident.name.to_string()));

            if let Some(initial_val) = &local.initial_val {
                let init_val = translate_literal(initial_val, &local.ty, builder);
                builder.mov(local_ref, init_val);
            }
        }
    }
}

fn build_func_body(
    body: &typ::ast::Block,
    return_ty: &Type,
    mut builder: Builder,
) -> Vec<Instruction> {
    let body_block_out_ref = match return_ty {
        Type::Nothing => Ref::Discard,
        _ => RETURN_REF.clone(),
    };

    translate_block(&body, body_block_out_ref, &mut builder);

    let mut instructions = builder.finish();

    // all functions should finish with the reserved EXIT label but to
    // avoid writing unused label instructions, if none of the other instructions in the body
    // are jumps to the exit label, we can elide it
    if jmp_exists(&instructions, EXIT_LABEL) {
        instructions.push(Instruction::Label(EXIT_LABEL));
    }

    instructions
}

pub fn build_static_closure_impl(
    closure: ClosureInstance,
    id: StaticClosureID,
    library: &mut LibraryBuilder,
) -> StaticClosure {
    let mut init_builder = Builder::new(library);

    let static_closure_ptr_ref = Ref::Global(GlobalRef::StaticClosure(id));

    let closure_ref = init_builder.build_closure_instance(closure.clone());
    init_builder.retain(closure_ref.clone(), &closure.closure_ptr_ty());
    init_builder.mov(static_closure_ptr_ref, closure_ref);

    let init_body = init_builder.finish();
    
    let debug_name = if library.opts().debug {
        Some(format!("static closure init for {}", closure))
    } else {
        None
    };

    let init_func_id = library.metadata_mut().insert_func(None);
    library.insert_func(
        init_func_id,
        Function::Local(FunctionDef {
            body: init_body,
            debug_name,
            sig: FunctionSig {
                param_tys: Vec::new(),
                return_ty: Type::Nothing,
            },
        }),
    );

    StaticClosure {
        id,
        init_func: init_func_id,
        closure_id: closure.closure_id,
        func_ty_id: closure.func_ty_id,
    }
}
