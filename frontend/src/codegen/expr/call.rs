use crate::codegen::builder::Builder;
use crate::codegen::typ;
use crate::codegen::expr;
use crate::codegen::FunctionInstance;
use crate::ir;
use crate::typ::InvocationValue;
use crate::typ::Specializable as _;
use crate::typ::Value;
use std::borrow::Cow;

fn translate_args(
    args: &[typ::ast::Expr],
    sig: &typ::FunctionSig,
    is_instance_method: bool,
    builder: &mut Builder,
) -> Vec<ir::Value> {
    let mut arg_vals = Vec::with_capacity(args.len());

    for (arg_index, (arg, param)) in args.iter().zip(sig.params.iter()).enumerate() {
        let arg_ref = expr::translate_expr(arg, builder);

        // for value types (any non RC), the self parameter of an instance method is invisibly
        // turned into a pointer to the type, so we need to pass it by its address here
        let is_value_type_method_self_arg =
            arg_index == 0 && is_instance_method && !sig.params[0].ty.is_strong_rc_reference();

        let arg_expr = if is_value_type_method_self_arg || param.is_by_ref() {
            let arg_ty = builder.translate_type(&arg.annotation().ty());

            let arg_ptr = builder.local_temp(arg_ty.ptr());
            builder.addr_of(arg_ptr.clone(), arg_ref);

            arg_ptr
        } else {
            arg_ref
        };

        arg_vals.push(ir::Value::from(arg_expr));
    }

    arg_vals
}

fn translate_call_with_args(
    call_target: CallTarget,
    args: &[typ::ast::Expr],
    sig: &typ::FunctionSig,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    let out_val = match &sig.result_ty {
        typ::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.translate_type(return_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            Some(out_val)
        },
    };

    builder.begin_scope();

    let is_instance_method = matches!(call_target, CallTarget::InstanceMethod(..));

    let mut arg_vals = translate_args(args, sig, is_instance_method, builder);

    if let CallTarget::Closure { closure_ptr, .. } = &call_target {
        arg_vals.insert(0, closure_ptr.clone());
    }

    match call_target {
        CallTarget::Closure { function, .. }
        | CallTarget::Function(function)
        | CallTarget::InstanceMethod(function) => {
            builder.call(function, arg_vals.clone(), out_val.clone());
        },

        CallTarget::Virtual {
            iface_id,
            iface_method_id,
        } => {
            let self_arg = arg_vals[0].clone();
            let rest_args = arg_vals[1..].to_vec();

            builder.vcall(
                iface_id,
                iface_method_id,
                self_arg,
                rest_args,
                out_val.clone(),
            );
        },
    }

    // no need to retain, the result of a function must be retained as part of its body

    builder.end_scope();

    out_val
}

enum CallTarget {
    Function(ir::Value),
    InstanceMethod(ir::Value),
    Closure {
        function: ir::Value,
        closure_ptr: ir::Value,
    },
    Virtual {
        iface_id: ir::InterfaceID,
        iface_method_id: ir::MethodID,
    },
}

pub fn build_call(call: &typ::ast::Call, builder: &mut Builder) -> Option<ir::Ref> {
    // eprintln!("build_call: {} @ {}", call, call.span());

    let Value::Invocation(invocation) = call.annotation() else {
        panic!(
            "bad value for call expression `{}`: {}",
            call,
            call.annotation()
        );
    };

    match invocation.as_ref() {
        InvocationValue::Function {
            function,
            args,
            type_args,
            ..
        } => {
            let func = builder.translate_func(&function.name, &function.sig, type_args.clone());

            let func_val = ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::Function(func.id)));

            let call_target = CallTarget::Function(func_val);

            translate_call_with_args(call_target, args, &func.sig, builder)
        },

        InvocationValue::Method {
            method,
            args,
            type_args,
            ..
        } => {
            // eprintln!("build_method_call: {} ({}){} = {}", method_call, method_call.iface_type, method_call.self_type, method_call.iface_method_index);
            build_method_call(
                method.self_ty.ty().clone(),
                method.self_ty.ty().clone(),
                method.index,
                args,
                type_args.clone(),
                builder,
            )
        },
        InvocationValue::VirtualMethod { .. } => unimplemented!(),
        InvocationValue::ObjectCtor { .. } => unimplemented!(),
        InvocationValue::VariantCtor { .. } => unimplemented!(),
        InvocationValue::FunctionValue { .. } => unimplemented!(),
    }
}

fn build_func_call(
    target: &typ::ast::Expr,
    args: &[typ::ast::Expr],
    call_ty_args: Option<typ::TypeArgList>,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    match target.annotation() {
        // calling a function directly
        typ::Value::Function(_func) => {
            unreachable!()
        },

        typ::Value::UfcsFunction(func) => {
            let func_instance = builder.translate_func(&func.function_name, &func.sig, None);
            let func_val = ir::Value::from(func_instance.id);

            let call_target = CallTarget::Function(func_val);

            let mut args_with_self_arg = Vec::with_capacity(args.len() + 1);
            args_with_self_arg.push((*func.self_arg).clone());
            args_with_self_arg.extend(args.iter().cloned());

            translate_call_with_args(
                call_target,
                &args_with_self_arg,
                &func_instance.sig,
                builder,
            )
        },

        // invoking a closure value that refers to a function invocation
        typ::Value::Typed(val) => {
            build_func_val_invocation(target, &val.ty, args, call_ty_args, builder)
        },

        // invoking a closure returned by a function
        typ::Value::Invocation(invocation) => build_func_val_invocation(
            target,
            invocation.result_type(),
            args,
            call_ty_args,
            builder,
        ),

        unexpected => panic!(
            "type of function call expr must be a function or callable value got: {:#?}",
            unexpected
        ),
    }
}

fn build_func_val_invocation(
    func_expr: &typ::ast::Expr,
    func_ty: &typ::Type,
    args: &[typ::ast::Expr],
    call_ty_args: Option<typ::TypeArgList>,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    // it's impossible to invoke a closure with type args, so the typechecker should
    // ensure this never happens
    assert!(
        call_ty_args.is_none(),
        "closure invocation cannot include type args"
    );

    // expr that evaluates to a closure pointer
    let target_expr_val = expr::translate_expr(func_expr, builder);
    let func_sig = func_ty.as_func().expect("target value of invocation must have function type");
    let func_ty_id = builder.translate_func_ty(&func_sig);

    // retrieve the actual function value
    let func_field_ptr = builder.local_temp(ir::Type::Function(func_ty_id).ptr());

    builder.scope(|builder| {
        let closure_ptr_ty = ir::Type::RcPointer(ir::VirtualTypeID::Closure(func_ty_id));
        builder.field(
            func_field_ptr.clone(),
            target_expr_val.clone(),
            closure_ptr_ty,
            ir::CLOSURE_PTR_FIELD,
        );
    });

    let call_target = CallTarget::Closure {
        function: func_field_ptr.to_deref().into(),
        closure_ptr: target_expr_val.clone().into(),
    };

    translate_call_with_args(call_target, args, &func_sig, builder)
}

fn build_method_call(
    iface_ty: typ::Type,
    self_ty: typ::Type,
    method_index: usize,
    args: &[typ::ast::Expr],
    ty_args: Option<typ::TypeArgList>,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    let iface_ty = builder.generic_context().apply_to_type(iface_ty);
    let self_ty = builder.generic_context().apply_to_type(self_ty);

    // for static methods, the self-type is nothing, and the interface type is the real type
    let method_decl_ty = if self_ty == typ::Type::Nothing {
        &iface_ty
    } else {
        &self_ty
    };

    let method_decl_index = builder.get_impl_method_index(method_decl_ty, &iface_ty, method_index);
    let method_decl = builder.get_method(method_decl_ty, method_decl_index);

    let method_decl_sig = method_decl.func_decl.sig().with_self(&self_ty);

    let call_generic_ctx = if let Some(ty_args_list) = &ty_args {
        let ty_params_list = method_decl
            .func_decl
            .name
            .type_params
            .as_ref()
            .expect("call with type args must have type params");

        Cow::Owned(builder.generic_context().child_context(ty_params_list, ty_args_list))
    } else {
        Cow::Borrowed(builder.generic_context())
    };

    let method_call_sig = call_generic_ctx.apply_to_sig(&method_decl_sig);

    let call_target = match builder.translate_type(&self_ty) {
        ir::Type::RcPointer(ir::VirtualTypeID::Interface(iface_id)) => {
            if ty_args.is_some() {
                unimplemented!("IR for virtual call with type args")
            }

            CallTarget::Virtual {
                iface_id,
                iface_method_id: ir::MethodID(method_decl_index),
            }
        },

        _ => {
            // eprintln!("method call ({}){}.{}: invoking method {}", iface_ty, self_ty, method_decl.func_decl.ident(), method_decl_index);

            let func_instance =
                builder.translate_method(method_decl_ty.clone(), method_decl_index, ty_args);

            let func_val = ir::Ref::from(func_instance.id);

            CallTarget::InstanceMethod(func_val.into())
        },
    };

    translate_call_with_args(call_target, &args, &method_call_sig, builder)
}

fn build_variant_ctor_call(
    variant_ty: &typ::Type,
    case_name: &str,
    arg: Option<&typ::ast::Expr>,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    let variant_ty =
        variant_ty.clone().apply_type_args(builder.generic_context(), builder.generic_context());

    let variant_name = variant_ty.as_variant().unwrap();

    let out_ty = builder.translate_type(&variant_ty);
    let out = builder.local_new(out_ty.clone(), None);

    builder.begin_scope();

    let tag_ptr = builder.local_temp(ir::Type::I32.ptr());
    builder.vartag(tag_ptr.clone(), out.clone(), out_ty.clone());

    let (_, case_index, _) = builder.translate_variant_case(variant_name, &case_name);

    // todo: proper index type
    builder.mov(tag_ptr.to_deref(), ir::Value::LiteralI32(case_index as i32));

    if let Some(arg) = arg {
        let arg_val = expr::expr_to_val(arg, builder);

        let arg_ty = builder.translate_type(&arg.annotation().ty());
        let field_ptr = builder.local_temp(arg_ty.clone().ptr());

        builder.vardata(field_ptr.clone(), out.clone(), out_ty.clone(), case_index);
        builder.mov(field_ptr.clone().to_deref(), arg_val);
        builder.retain(field_ptr.to_deref(), &arg_ty);
    }

    builder.end_scope();
    Some(out)
}

fn build_func_invocation(
    args: &[typ::ast::Expr],
    func: FunctionInstance,
    is_instance_method: bool,
    builder: &mut Builder
) -> Option<ir::Ref> {
    let result = if func.sig.result_ty == typ::Type::Nothing {
        None
    } else {
        let result_ty = builder.translate_type(&func.sig.result_ty);
        Some(builder.local_new(result_ty, None))
    };

    builder.scope(|builder| {
        let arg_values = translate_args(args, &func.sig, is_instance_method, builder);

        let func_val = ir::Ref::Global(ir::GlobalRef::Function(func.id));

        builder.call(func_val, arg_values, result.clone());
    });
    
    result
}

pub fn translate_invocation(
    invocation: &InvocationValue,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    match invocation {
        InvocationValue::Function {
            function,
            type_args,
            ..
        } => {
            let func = builder.translate_func(&function.name, &function.sig, type_args.clone());

            build_func_invocation(invocation.args(), func, false, builder)
        },

        InvocationValue::Method {
            method, type_args, ..
        } => {
            let func = builder.translate_method(
                method.self_ty.ty().clone(),
                method.index,
                type_args.clone(),
            );

            build_func_invocation(invocation.args(), func, true, builder)
        },

        InvocationValue::VirtualMethod { method, .. } => {
            assert!(
                method.self_ty.is_strong_rc_reference(),
                "virtual call self type must be a class or interface pointer"
            );

            unimplemented!()
        },

        InvocationValue::VariantCtor { variant_type, case, arg, .. } => {
            build_variant_ctor_call(variant_type.ty(), case.as_str(), arg.as_ref(), builder)
        },

        _ => unimplemented!(),
    }
}
