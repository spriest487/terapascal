use crate::ast;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_block;
use crate::codegen::typ;
use crate::codegen::ClosureInstance;
use crate::codegen::IRBuilder;
use crate::ir;
use std::iter;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_ir::InstructionBuilder;
use terapascal_ir::MetadataSource;

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: ir::FunctionID,
    pub src_sig: Arc<typ::FunctionSig>,

    pub published: bool,
}

fn create_function_body_builder<'m, 'l: 'm>(
    lib: &'m mut LibraryBuilder<'l>,
    debug_name: Option<String>,
) -> IRBuilder<'m, 'l> {
    let debug_comment = debug_name.map(|name| {
        format!("function def body of {}", name)
    });

    let mut builder = IRBuilder::new(lib);
    
    if let Some(comment) = debug_comment {
        builder.comment(&comment);
    }

    builder
}

pub fn build_func_def(
    module: &mut LibraryBuilder,
    def_params: &[typ::ast::FunctionParamGroup],
    def_type_params: Option<&typ::TypeParamList>,
    def_return_ty: &typ::Type,
    def_body: &typ::ast::Block,
    def_locals: &[typ::ast::FunctionLocalBinding],
    is_instance_method: bool,
    enclosing_type: Option<&typ::Type>,
    debug_name: Option<String>,
) -> ir::FunctionDef {
    let mut body_builder = create_function_body_builder(
        module,
        debug_name.clone()
    );

    let return_ty = bind_function_return(def_return_ty, &mut body_builder);

    let bind_params: Vec<_> = def_params
        .iter()
        .flat_map(|param| FunctionParam::from_ast(param, &mut body_builder))
        .collect();
    let bound_params = bind_function_params(bind_params, is_instance_method, ir::ArgID(0), &mut body_builder);

    let mut type_params = Vec::new();

    // if the self param is a specialized generic name, add its type parameters to the
    // beginning of the method function's type param list
    if let Some(enclosing_params) = enclosing_type.and_then(|t| t.type_params()) {
        for param in &enclosing_params.items {
            let param_name = Arc::new(param.name.name.to_string());
            type_params.push(param_name);
        }
    }

    type_params.extend(def_type_params
        .into_iter()
        .flat_map(|param_list| param_list.items
            .iter()
            .map(|param| Arc::new(param.name.to_string())))
    );

    init_function_locals(def_locals, &mut body_builder);
    let body = build_func_body(def_body, &return_ty, body_builder);

    ir::FunctionDef {
        body,
        sig: Rc::new(ir::FunctionSig {
            param_types: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            result_type: return_ty,
        }),
        type_params,
    }
}

pub fn build_func_static_closure_def(
    library: &mut LibraryBuilder,
    target_func: &FunctionInstance,
    closure_sig: &Rc<ir::FunctionSig>,
    target_func_id: ir::FunctionID,
) -> ir::FunctionDef {
    let params = target_func
        .src_sig
        .params
        .iter()
        .enumerate()
        .map(|(index, sig_param)| {
            FunctionParam {
                by_ref: matches!(sig_param.modifier, Some(ast::FunctionParamMod::Var | ast::FunctionParamMod::Out)),
                ty: library.translate_type(&sig_param.ty),
                name: format!("P{index}"),
            }
        })
        .collect::<Vec<_>>();

    let debug_name = match library.metadata().func_desc(target_func_id) {
        Some(func_name) if library.opts().debug => {
            Some(format!("static closure def ({func_name})"))
        },

        _ => None,
    };

    let mut body_builder = create_function_body_builder(library, debug_name.clone());

    let return_ty = bind_function_return(&target_func.src_sig.result_ty, &mut body_builder);

    // this method needs to be compatible with the type-erased function pointer stored in a
    // closure struct, which has the sig "(Object, ...actual params)"
    let closure_ptr_arg = ir::ArgID(0);
    let mut bound_params = vec![(closure_ptr_arg, closure_sig.to_closure_ptr_type())];

    // bind the closure pointer arg at ID 0
    body_builder.bind_closure_ptr(closure_ptr_arg, closure_sig);
    
    // bind the rest of the args at ID 1+
    bound_params.extend(bind_function_params(params, false, ir::ArgID(1), &mut body_builder));

    let func_global = ir::Ref::Global(ir::GlobalRef::func(target_func.id, []));

    // this is a static closure, so we ignore the closure pointer (it's static) and just pass
    // the rest of the args in IDs 1+ as the args to the real function
    let func_args: Vec<ir::Value> = bound_params
        .iter()
        .skip(1)
        .map(|(id, _)| id.value())
        .collect();

    let return_ref = match return_ty {
        ir::Type::Nothing => None,
        _ => Some(ir::RESULT_REF),
    };

    body_builder.call(func_global, func_args, return_ref);

    ir::FunctionDef {
        sig: Rc::new(ir::FunctionSig {
            param_types: bound_params
                .into_iter()
                .map(|(_, param_ty)| param_ty)
                .collect(),
            result_type: return_ty,
        }),
        type_params: Vec::new(),
        body: body_builder.finish(),
    }
}

pub fn build_closure_function_def(
    lib: &mut LibraryBuilder,
    func_def: &typ::ast::AnonymousFunctionDef,
    closure_id: ir::TypeDefID,
    closure_sig: &Rc<ir::FunctionSig>,
    debug_name: Option<String>,
) -> ir::FunctionDef {
    let closure_def = lib.metadata().get_struct_def(closure_id).cloned().unwrap();

    let mut body_builder = create_function_body_builder(lib, debug_name.clone());

    let return_ty = bind_function_return(&func_def.result_ty, &mut body_builder);

    // the type-erased pointer to the closure struct is included as the 0th param but
    // *not* bound like a normal param since it can't be named from code, so bind it in the scope
    // of this function body now
    let closure_arg = ir::ArgID(0);
    body_builder.bind_closure_ptr(closure_arg, closure_sig);

    let def_params: Vec<_> = func_def.params
        .iter()
        .flat_map(|param| FunctionParam::from_ast(param, &mut body_builder))
        .collect();

    let bound_params = bind_function_params(def_params, false, ir::ArgID(1), &mut body_builder);

    // cast the closure pointer param from the erased pointer passed in to its actual class type
    let closure_ptr_ty = closure_id.to_class_ptr_type([]);
    let closure_ptr_ref = body_builder.local_temp(closure_ptr_ty.clone());
    body_builder.cast(
        closure_ptr_ref.clone(),
        closure_arg.value(),
        closure_ptr_ty.clone(),
    );

    // copy closure members into the body scope
    // the order doesn't need to match the closure struct field order (although it probably
    // will), we just need to ensure all captures are bound to unique locals before we
    // start letting the body code allocate its own locals
    for (field_id, field_def) in closure_def.fields.iter() {
        if *field_id == ir::CLOSURE_PTR_FIELD {
            continue;
        }

        let field_name = match field_def.name.as_ref() {
            None => continue,
            Some(name) => name,
        };

        let capture_val_ptr_field_ref = body_builder.local_closure_capture(
            field_def.ty.clone(), 
            field_name.clone()
        );

        let closure_ptr_field_ref = closure_ptr_ref.to_ref().field_ref(closure_ptr_ty.clone(), *field_id);

        body_builder.make_ref(
            capture_val_ptr_field_ref,
            closure_ptr_field_ref.to_deref()
        );
    }

    let body = build_func_body(&func_def.body, &return_ty, body_builder);

    // the 0th parameter of the function is always a type-erased pointer, which we must
    // cast to the actual closure struct type in the body
    let actual_params = iter::once(closure_sig.to_closure_ptr_type())
        .chain(bound_params.into_iter().map(|(_, param_ty)| param_ty))
        .collect();

    ir::FunctionDef {
        body,
        sig: Rc::new(ir::FunctionSig {
            param_types: actual_params,
            result_type: return_ty,
        }),
        type_params: Vec::new(),
    }
}

fn bind_function_return(return_ty: &typ::Type, builder: &mut IRBuilder) -> ir::Type {
    match return_ty {
        typ::Type::Nothing => ir::Type::Nothing,
        
        return_ty => {
            let return_ty = builder.translate_type(return_ty);

            builder.comment(format!("result binding: {}", builder.pretty_ty_name(&return_ty)));

            builder.bind_result(return_ty.clone());
            return_ty
        },
    }
}

#[derive(Debug, Clone)]
struct FunctionParam {
    pub name: String,
    pub ty: ir::Type,
    pub by_ref: bool,
}

impl FunctionParam {
    fn from_ast(param: &typ::ast::FunctionParamGroup, builder: &mut IRBuilder) -> Vec<Self> {
        let mut params = Vec::new();
        
        let (param_ty, by_ref) = match param.get_modifier() {
            Some(ast::FunctionParamMod::Var) | Some(ast::FunctionParamMod::Out) => {
                (builder.translate_type(&param.ty), true)
            },

            None => (builder.translate_type(&param.ty), false),
        };

        for item in &param.param_items {
            let name = item.name.to_string();

            params.push(FunctionParam {
                name,
                by_ref,
                ty: param_ty.clone(),
            });
        };

        params
    }
}

fn bind_function_params(
    params: impl IntoIterator<Item=FunctionParam>,
    is_instance_method: bool,
    first_id: ir::ArgID,
    builder: &mut IRBuilder,
) -> Vec<(ir::ArgID, ir::Type)> {
    let mut bound_params = Vec::new();

    // for instance methods, the first arg is the self pointer or ref
    let mut is_self_param = is_instance_method;
    
    let mut id = first_id;
    for param in params.into_iter() {
        let mut by_ref = param.by_ref;

        // pass the self parameter as a ref for value types
        if is_self_param {
            if !param.ty.is_object() {
                assert!(!by_ref, "self param should not already be by-ref");
                by_ref = true;
            }

            is_self_param = false;
        }

        let bound_ty = if by_ref {
            let ref_ty = param.ty.clone().temp_ref();
            builder.bind_ref_param(id, param.ty, &param.name);
            
            ref_ty
        } else {
            let bound_ty = param.ty.clone();
            builder.bind_param(id, param.ty, &param.name);
            
            bound_ty
        };

        builder.comment(&format!("param: {} = {}", id, builder.pretty_ty_name(&bound_ty)));
        bound_params.push((id, bound_ty));

        id.0 += 1;
    }

    bound_params
}

fn init_function_locals(locals: &[typ::ast::FunctionLocalBinding], builder: &mut IRBuilder) {
    for local in locals {
        if local.kind == ast::BindingDeclKind::Var {
            let ty = builder.translate_type(&local.ty);

            let local_name = Arc::new(local.ident.name.to_string());
            let local_ref = builder.local_var(ty, Some(local_name));

            if let Some(initial_val) = &local.initial_val {
                let init_val = builder.literal_to_val(initial_val, &local.ty);
                builder.mov(local_ref, init_val);
            }
        }
    }
}

fn build_func_body(
    body: &typ::ast::Block,
    return_ty: &ir::Type,
    mut builder: IRBuilder,
) -> ir::InstructionList {
    let body_block_out_ref = match return_ty {
        ir::Type::Nothing => ir::Ref::Discard,
        _ => ir::RESULT_REF.clone(),
    };

    translate_block(&body, body_block_out_ref, &mut builder);

    let mut body = builder.finish();

    // all functions should finish with the reserved EXIT label but to
    // avoid writing unused label instructions, if none of the other instructions in the body
    // are jumps to the exit label, we can elide it
    if ir::util::jmp_exists(&body.instructions, ir::EXIT_LABEL) {
        body.push(ir::Instruction::Label(ir::EXIT_LABEL), None);
    }

    body
}

pub fn build_static_closure_impl(
    closure: ClosureInstance,
    id: ir::VariableID,
    library: &mut LibraryBuilder,
) -> ir::StaticClosure {
    let mut init_builder = IRBuilder::new(library);

    let static_closure_ptr_ref = ir::Ref::Global(ir::GlobalRef::Variable(id));

    let closure_ref = init_builder.build_closure_value(closure.clone(), true);
    init_builder.cast(static_closure_ptr_ref, closure_ref, closure.function_pointer_type());

    let init_body = init_builder.finish();

    let internal_name = format!("static closure init for {}", closure);
    let identity = ir::FunctionIdentity::internal(internal_name);

    let init_sig = Rc::new(ir::FunctionSig {
        param_types: Vec::new(),
        result_type: ir::Type::Nothing,
    });

    let init_func_id = library
        .metadata_mut()
        .insert_func(identity, init_sig.clone(), false, []);

    library.insert_function(
        init_func_id,
        ir::Function::Local(ir::FunctionDef {
            body: init_body,
            sig: init_sig,
            type_params: Vec::new(),
        }),
    );

    ir::StaticClosure {
        id,
        identity: ir::ClosureIdentity {
            sig: closure.sig,
            id: closure.func_instance.id,
        },
        init_func: init_func_id,
        closure_id: closure.closure_id,
    }
}