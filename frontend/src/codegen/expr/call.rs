use crate::codegen::builder::IRBuilder;
use crate::codegen::expr;
use crate::codegen::expr::ctor::build_object_ctor_invocation;
use crate::codegen::typ;
use crate::ir;
use crate::typ::ast::apply_func_decl_ty_args;
use crate::typ::GenericContext;
use std::sync::Arc;
use terapascal_ir::InstructionBuilder;

fn translate_args(
    args: &[typ::ast::Expr],
    sig: &typ::FunctionSig,
    is_instance_method: bool,
    builder: &mut IRBuilder,
) -> Vec<ir::Value> {
    let mut arg_vals = Vec::with_capacity(args.len());

    for (arg_index, (arg, param)) in args.iter().zip(sig.params.iter()).enumerate() {
        let arg_ref = expr::translate_expr(arg, builder);

        // for value types (any non RC), the self parameter of an instance method is invisibly
        // turned into a pointer to the type, so we need to pass it by its address here
        let is_value_type_method_self_arg = arg_index == 0 
            && is_instance_method 
            && !sig.params[0].ty.is_object();

        let arg_expr = if is_value_type_method_self_arg || param.is_by_ref() {
            let arg_ty = builder.translate_type(&arg.annotation().ty());

            builder.comment(&format!("by-ref param ({})", param.ty));
            let arg_ref_var = builder.local_temp(arg_ty.temp_ref());
            builder.make_ref(arg_ref_var, arg_ref);

            arg_ref_var.to_ref()
        } else {
            arg_ref
        };

        arg_vals.push(ir::Value::from(arg_expr));
    }

    arg_vals
}

fn translate_call_type_args(
    type_args: Option<&typ::TypeArgList>,
    builder: &mut IRBuilder,
) -> Vec<ir::Type> {
    let mut types = Vec::new();

    if let Some(type_arg_list) = type_args {
        types.reserve(type_arg_list.len());

        for t in type_arg_list.iter() {
            types.push(builder.translate_type(t.ty()));
        }
    }

    types
}

fn translate_call_with_args(
    call_target: CallTarget,
    args: &[typ::ast::Expr],
    type_args: Option<&typ::TypeArgList>,
    sig: &typ::FunctionSig,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    let out_val = match &sig.result_ty {
        typ::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.translate_type(return_ty);
            let out_val = builder.local_var(out_ty.clone(), None).to_ref();
            Some(out_val)
        },
    };

    builder.local_begin();
    {
        // don't include virtual methods in this, the vcall instruction takes its first
        // argument as a ref already
        let is_instance_method = matches!(
            call_target,
            CallTarget::InstanceMethod(..)
        );

        let type_args = translate_call_type_args(type_args, builder);

        let mut arg_vals = translate_args(args, sig, is_instance_method, builder);

        if let CallTarget::Closure { closure_ptr, .. } = &call_target {
            arg_vals.insert(0, closure_ptr.clone());
        }

        match call_target {
            | CallTarget::Function(function)
            | CallTarget::InstanceMethod(function) => {
                let func_ref = ir::FunctionRef::new(function).with_args(type_args);
                builder.call(func_ref, arg_vals.clone(), out_val.clone());
            },

            CallTarget::Closure { function, .. } => {
                assert!(type_args.is_empty());
                builder.call(function, arg_vals.clone(), out_val.clone());
            },

            CallTarget::Virtual {
                iface_id,
                iface_method_id,
            } => {
                // for a virtual call, the self-arg is a ref. if the value provided is already a
                // ref, use that, but if not (e.g. calling a method on a const or temp value),
                // just store it in a temporary local var and use that
                let self_arg = match &arg_vals[0] {
                    ir::Value::Ref(self_ref) => self_ref.clone(),

                    temp_val => {
                        let self_type = builder.translate_type(args[0].annotation().ty().as_ref());
                        let self_var = builder.local_temp(self_type);

                        builder.mov(self_var, temp_val.clone());

                        self_var.to_ref()
                    }
                };

                let rest_args = arg_vals[1..].to_vec();

                assert_eq!(0, type_args.len(), "not supported yet: generic virtual call");

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
    }
    builder.local_end();

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

pub fn build_call(call: &typ::ast::Call, builder: &mut IRBuilder) -> Option<ir::Ref> {
    // eprintln!("build_call: {} @ {}", call, call.span());

    let typ::Value::Invocation(invocation) = &call.annotation else {
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
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    // expr that evaluates to a closure pointer
    let target_expr_val = expr::translate_expr(func_expr, builder);
    let func_sig = func_ty.as_func().expect("target value of invocation must have function type");
    let sig = builder.translate_sig(&func_sig);

    // retrieve the actual function value
    let closure_ptr_ty = sig.into_closure_ptr_type();
    let func_field_ref = target_expr_val.clone().field_ref(closure_ptr_ty, ir::CLOSURE_PTR_FIELD);

    let call_target = CallTarget::Closure {
        function: func_field_ref.to_deref().value(),
        closure_ptr: target_expr_val.clone().into(),
    };

    translate_call_with_args(call_target, args, None, &func_sig, builder)
}

pub fn build_method_invocation(
    iface_ty: typ::Type,
    self_ty: typ::Type,
    method_index: usize,
    args: &[typ::ast::Expr],
    ty_args: Option<typ::TypeArgList>,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    // for static methods, the self-type is nothing, and the interface type is the real type
    let method_decl_ty = if self_ty == typ::Type::Nothing {
        &iface_ty
    } else {
        &self_ty
    };

    let (method_decl_ty, method_decl_index) = builder.get_impl_method_index(method_decl_ty, &iface_ty, method_index);

    let method_decl = builder.get_method(method_decl_ty, method_decl_index);
    let method_decl_sig = method_decl.func_decl.sig().with_self(&self_ty);

    // eprintln!("## method invocation");
    // eprintln!("  iface_ty = {iface_ty}");
    // eprintln!("   self_ty = {self_ty}");
    // eprintln!("   decl ty = {method_decl_ty}");
    // eprintln!("   method = {}", method_decl.func_decl);

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

        GenericContext::new(ty_params_list, ty_args_list)
    } else {
        GenericContext::empty()
    };

    let call_target = match builder.translate_type(&method_decl_ty) {
        ir::Type::Object(ir::ObjectID::Interface(iface_id)) => {
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

            let self_type = method_decl_ty.clone();
            let func_instance = builder.translate_method(self_type, method_decl_index);

            // static methods are translated to free functions
            if self_ty == typ::Type::Nothing {
                CallTarget::Function(func_instance.id)
            } else {
                CallTarget::InstanceMethod(func_instance.id)
            }
        },
    };

    let method_call_sig = call_generic_ctx.apply_to_sig(&method_decl_sig);

    let enclosing_type_args = method_decl_ty
        .full_name()
        .and_then(|name| name.type_args.clone());

    // methods are translated into IR functions which have a combined type parameter list
    // where the enclosing type params are followed by the method's own type params, so we
    // need to combine both lists here for an invocation
    let method_call_type_args = match (&call_target, enclosing_type_args, ty_args) {
        (CallTarget::Virtual { .. }, _, invocation_args) => {
            // virtual method calls are associated with a (maybe generic) type, so we don't need
            // to duplicate that type's type args in the call args
            invocation_args
        }

        (_, Some(enclosing_args), None) => {
            Some(enclosing_args)
        }
        (_, Some(enclosing_args), Some(call_args)) => {
            let mut combined_args = enclosing_args.clone();
            combined_args.items.extend(call_args.items.into_iter());
            Some(combined_args)
        }
        (_, None, call_args) => {
            call_args
        },
    };

    translate_call_with_args(call_target, &args, method_call_type_args.as_ref(), &method_call_sig, builder)
}

fn build_variant_ctor_call(
    variant_ty: &typ::Type,
    case_name: &str,
    arg: Option<&typ::ast::Expr>,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    let variant_name = variant_ty.as_variant().unwrap();

    let out_ty = builder.translate_type(&variant_ty);
    let out = builder.local_var(out_ty.clone(), None);

    builder.local_begin();
    {
        let (_, case_index, _) = builder.translate_variant_case(variant_name, &case_name);

        // todo: proper index type
        let tag_ref = out.to_ref().vartag_ref(out_ty.clone());
        builder.mov(tag_ref.to_deref(), ir::Value::LiteralI32(case_index as i32));

        if let Some(arg) = arg {
            let arg_val = expr::expr_to_val(arg, builder);

            let arg_ty = builder.translate_type(&arg.annotation().ty());
            let data_ref = out.to_ref().vardata_ref(out_ty.clone(), case_index);

            builder.mov(data_ref.to_deref(), arg_val);
            builder.retain(data_ref.to_deref(), arg_ty);
        }
    }
    builder.local_end();

    Some(out.to_ref())
}

pub fn translate_invocation(
    invocation: &typ::Invocation,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    match invocation {
        typ::Invocation::Function {
            function,
            type_args,
            ..
        } => {
            // eprintln!("translating func invocation of: {} ({})", function.name, function.sig);
            
            // this needs to be the name and signature *as declared*, not the invoked signature,
            // so the function builder can apply the type args to the source type properly
            let decl_sig = Arc::new(function.decl.sig());

            // the sig of the function as it's called, after generic specialization
            let invocation_sig = match type_args {
                Some(args) => {
                    let invoked_decl = apply_func_decl_ty_args(&function.decl, args);
                    Arc::new(invoked_decl.sig())
                }
                None => {
                    decl_sig.clone()
                },
            };

            let func = builder.translate_func(&function.name, &decl_sig);
            let args: Vec<_> = invocation.args()
                .cloned()
                .collect();

            let call_target = CallTarget::Function(func.id);
            translate_call_with_args(call_target, &args, type_args.as_ref(), &invocation_sig, builder)
        },

        typ::Invocation::Method {
            self_ty, method, args, type_args, .. 
        } => {
            assert_eq!(
                args.len(),
                method.decl.func_decl.params_len(),
                "argument count mismatch for {} at {}",
                method.decl.func_decl.name,
                method.span,
            );
            
            build_method_invocation(
                method.self_ty.clone(),
                self_ty.clone(),
                method.index,
                args.as_slice(),
                type_args.clone(),
                builder,
            )
        },

        typ::Invocation::VariantCtor { variant_type, case, arg, .. } => {
            build_variant_ctor_call(variant_type, case.as_str(), arg.as_ref(), builder)
        },

        typ::Invocation::ObjectCtor { object_type, members, .. } => {
            let ctor_result = build_object_ctor_invocation(object_type, members.as_slice(), builder);
            Some(ctor_result)
        }

        typ::Invocation::FunctionValue { value, args, .. } => {
            let value_ty = value.annotation().ty();
            build_func_val_invocation(value, value_ty.as_ref(), args, builder)
        }
    }
}
