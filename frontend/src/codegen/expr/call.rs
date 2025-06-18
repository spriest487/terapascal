use crate::codegen::builder::Builder;
use crate::codegen::expr;
use crate::codegen::expr::ctor::build_object_ctor_invocation;
use crate::codegen::typ;
use crate::ir;
use crate::typ::Invocation;
use crate::typ::Specializable as _;
use crate::typ::Value;
use std::borrow::Cow;
use std::sync::Arc;

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
        | CallTarget::Function(function)
        | CallTarget::InstanceMethod(function) => {
            builder.call(function, arg_vals.clone(), out_val.clone());
        },
        
        CallTarget::Closure { function, .. } => {
            builder.call(function, arg_vals.clone(), out_val.clone());
        },

        CallTarget::Virtual {
            iface_id,
            iface_method_id,
        } => {
            let self_arg = arg_vals[0].clone();
            let rest_args = arg_vals[1..].to_vec();

            // eprintln!("({sig}) => building vcall: {iface_id}.{} vs for type {self_ty}/{iface_ty}", iface_method_id.0);

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
    Function(ir::FunctionID),
    InstanceMethod(ir::FunctionID),
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

    let Value::Invocation(invocation) = &call.annotation else {
        panic!(
            "bad value for call expression `{}`: {}",
            call,
            call.annotation
        );
    };
    
    translate_invocation(invocation, builder)
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

pub fn build_method_invocation(
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
    // 
    // eprintln!("## method invocation");
    // eprintln!("  iface_ty = {iface_ty}");
    // eprintln!("   self_ty = {self_ty}");
    // eprintln!("   decl ty = {method_decl_ty}");

    let method_decl_index = builder.get_impl_method_index(method_decl_ty, &iface_ty, method_index);
    let method_decl = builder.get_method(method_decl_ty, method_decl_index);

    let method_decl_sig = method_decl.func_decl.sig().with_self(&self_ty);

    assert_eq!(
        args.len(), 
        method_decl_sig.params.len(), 
        "argument count mismatch for {}",
        method_decl.func_decl.name, 
    );

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

            CallTarget::InstanceMethod(func_instance.id)
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

pub fn translate_invocation(
    invocation: &Invocation,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    match invocation {
        Invocation::Function {
            function,
            type_args,
            ..
        } => {
            // eprintln!("translating func invocation of: {} ({})", function.name, function.sig);
            
            // this needs to be the name and signature *as declared*, not the invoked signature,
            // so the function builder can apply the type args to the source type properly
            let decl_sig = Arc::new(function.decl.sig());
            let func = builder.translate_func(&function.name, &decl_sig, type_args.clone());
            let args: Vec<_> = invocation.args()
                .cloned()
                .collect();

            let call_target = CallTarget::Function(func.id);
            translate_call_with_args(call_target, &args, &func.sig, builder)
        },

        Invocation::Method {
            self_ty, method, args, type_args, .. 
        } => {
            assert_eq!(
                args.len(),
                method.decl.func_decl.params.len(),
                "argument count mismatch for {} at {}",
                method.decl.func_decl.name,
                method.span,
            );
            
            build_method_invocation(
                method.self_ty.ty().clone(),
                self_ty.clone(),
                method.index,
                args.as_slice(),
                type_args.clone(),
                builder,
            )
        },

        Invocation::VariantCtor { variant_type, case, arg, .. } => {
            build_variant_ctor_call(variant_type.ty(), case.as_str(), arg.as_ref(), builder)
        },

        Invocation::ObjectCtor { object_type, members, .. } => {
            let ctor_result = build_object_ctor_invocation(object_type.ty(), members.as_slice(), builder);
            Some(ctor_result)
        }

        Invocation::FunctionValue { value, args, .. } => {
            let value_ty = value.annotation().ty();
            build_func_val_invocation(value, value_ty.as_ref(), args, None, builder)
        }
    }
}
