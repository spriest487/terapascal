use crate::ast;
use crate::typ;
use crate::typ::ast::Call;
use crate::typ::ast::Call;
use crate::typ::ast::Stmt;
use crate::typ::ast::StructDecl;
use crate::typ::context::*;
use crate::typ::ty::*;
use crate::typ::ModuleUnit;

const INT32: TypeName = TypeName::Inferred(Type::Primitive(Primitive::Int32));
const BYTE: TypeName = TypeName::Inferred(Type::Primitive(Primitive::UInt8));

fn module_from_src(unit_name: &'static str, src: &'static str) -> ModuleUnit {
    let module = typ::test::module_from_src(unit_name, src);
    module.units.into_iter().next().unwrap()
}

fn main_unit_structs(module: &typ::ModuleUnit) -> Vec<Arc<StructDecl>> {
    module.unit
        .type_decls()
        .flat_map(|(_vis, decl)| decl.items.iter())
        .map(|t| match t {
            ast::TypeDeclItem::Struct(c) => c.clone(),
            _ => unreachable!(),
        })
        .collect()
}

#[test]
fn specialize_class_has_correct_field_types() {
    let module = module_from_src(
        "specialize_class_has_correct_member_types",
        r"  
            implementation
            uses System;
            
            type A[T] = class
                t: T;
                x: UInt8;
            end;
            end
        ",
    );
    
    let tys = main_unit_structs(&module);

    let span = Span::zero("test");

    let type_args = TypeArgList::new(vec![INT32], span.clone());
    let result = specialize_struct_def(&tys[0], &type_args, &module.context).unwrap();

    assert!(result.name.type_args.is_some());
    let actual_type_args = result.name.type_args.as_ref().unwrap();

    assert_eq!(1, actual_type_args.len());
    assert_eq!(INT32, *actual_type_args.items[0].ty());

    assert_eq!(INT32, result.fields().nth(0).unwrap().ty);
    assert_eq!(BYTE, result.fields().nth(1).unwrap().ty);
}

#[test]
fn specialize_class_has_multi_correct_field_types() {
    let module = module_from_src(
        "specialize_class_has_multi_correct_member_types",
        r"
            implementation
            type A[T1, T2] = class
                t1: T1;
                t2: T2;
            end;
            end
        ",
    );
    let tys = main_unit_structs(&module);
    
    let span = Span::zero("test");

    let type_args = TypeArgList::new(vec![
        INT32, 
        BYTE,
    ], span.clone());

    let result = specialize_struct_def(&tys[0], &type_args, &module.context).unwrap();

    assert!(result.name.type_args.is_some());
    let actual_type_args = result.name.type_args.as_ref().unwrap();

    assert_eq!(2, actual_type_args.len());
    assert_eq!(INT32, *actual_type_args.items[0].ty());
    assert_eq!(BYTE, *actual_type_args.items[1].ty());

    assert_eq!(INT32, result.fields().nth(0).unwrap().ty);
    assert_eq!(BYTE, result.fields().nth(1).unwrap().ty);
}

#[test]
fn specialized_class_has_correct_non_generic_method_types() {
    let module = module_from_src(
        "test",
        r"  
            implementation
            uses System;
            
            type A[T] = class
                function M1(i: UInt8): UInt8;
            end;
            
            function A[T].M1(i: UInt8): UInt8; begin end;
            end
        ",
    );

    let tys = main_unit_structs(&module);

    let generic_def = module.context
        .find_struct_def(&tys[0].name.full_path, StructKind::Class)
        .unwrap();

    let methods: Vec<_> = generic_def.methods().collect();
    assert_eq!("test.A[T]", methods[0].func_decl.params[0].ty.to_string());
    assert_eq!("test.A[T]", methods[0].func_decl.method_declaring_type().unwrap().to_string());
    assert_eq!(BYTE, methods[0].func_decl.params[1].ty);
    assert_eq!(BYTE, methods[0].func_decl.result_ty);

    let type_args = TypeArgList::new(vec![INT32], builtin_span());
    let specialized_def = specialize_struct_def(&tys[0], &type_args, &module.context).unwrap();

    assert_eq!(1, specialized_def.name.type_args.as_ref().unwrap().len());
    assert_eq!(INT32, *specialized_def.name.type_args.as_ref().unwrap().items[0]);
    
    let methods: Vec<_> = specialized_def.methods().collect();
    assert_eq!("test.A[System.Int32]", methods[0].func_decl.method_declaring_type().unwrap().to_string());
    assert_eq!("test.A[System.Int32]", methods[0].func_decl.params[0].ty.to_string());
    assert_eq!(BYTE, methods[0].func_decl.params[1].ty);
    assert_eq!(BYTE, methods[0].func_decl.result_ty);
}

#[test]
fn specialized_class_has_correct_method_types_using_class_ty_params() {
    let module = module_from_src(
        "test",
        r"  
            implementation
            uses System;
            
            type A[T] = class
                function M2(x: T): T;
            end;
            
            function A[T].M2(x: T): T; begin end;
            
            end
        ",
    );

    let tys = main_unit_structs(&module);
    
    let generic_def = module.context
        .find_struct_def(&tys[0].name.full_path, StructKind::Class)
        .unwrap();
    
    let methods: Vec<_> = generic_def.methods().collect();
    assert_eq!("test.A[T]", methods[0].func_decl.params[0].ty.to_string());
    assert_eq!("T", methods[0].func_decl.params[1].ty.to_string());
    assert_eq!("T", methods[0].func_decl.result_ty.to_string());

    let type_args = TypeArgList::new(vec![INT32], builtin_span());
    let specialized_def = specialize_struct_def(&tys[0], &type_args, &module.context).unwrap();

    let methods: Vec<_> = specialized_def.methods().collect();
    assert_eq!("test.A[System.Int32]", methods[0].func_decl.params[0].ty.to_string());
    assert_eq!(INT32, methods[0].func_decl.params[1].ty);
    assert_eq!(INT32, methods[0].func_decl.result_ty);
}

#[test]
fn specialized_class_has_correct_method_types_with_method_ty_params() {
    let module = module_from_src(
        "specialize_class_has_correct_method_types",
        r"  
            implementation
            uses System;
            
            type A[T] = class
                function M3[U](x: T; i: UInt8): U;
            end;
            
            function A[T].M3[U](x: T; i: UInt8): U; begin end;
            
            end
        ",
    );

    let tys = main_unit_structs(&module);

    let type_args = TypeArgList::new(vec![INT32], builtin_span());
    let specialized_def = specialize_struct_def(&tys[0], &type_args, &module.context).unwrap();

    let self_ty = Type::class(specialized_def.name.clone());
    let generic_u = Type::generic_param(builtin_ident("U"));

    let methods: Vec<_> = specialized_def.methods().collect();
    assert_eq!(self_ty, methods[0].func_decl.params[0].ty);
    assert_eq!(INT32, methods[0].func_decl.params[1].ty);
    assert_eq!(BYTE, methods[0].func_decl.params[2].ty);
    assert_eq!(generic_u, methods[0].func_decl.result_ty);
}

#[test]
fn specialize_class_with_deep_params() {
    const UNIT_NAME: &str ="specialize_class_with_deep_params"; 
    let module = module_from_src(
        UNIT_NAME,
        r"  
            implementation
            uses System;
            
            type A[T1, T2] = record
                t1: T1;
                t2: T2;
            end;
            
            type B[T] = class
                a: A[T, T];
                b: T;
            end;
            end
        ",
    );
    
    let tys = main_unit_structs(&module);

    let span = Span::zero(UNIT_NAME);

    let type_args = TypeArgList::new(vec![INT32], span.clone());

    let result = specialize_struct_def(&tys[1], &type_args, &module.context).unwrap();

    let a_name = result.fields().nth(0).unwrap().ty.as_record().unwrap();

    assert_eq!(format!("{UNIT_NAME}.A[System.Int32, System.Int32]"), a_name.to_string());
    assert_eq!(INT32, result.fields().nth(1).unwrap().ty);
}

#[test]
fn specialized_fn_has_right_sig() {
    let module = module_from_src(
        "specialized_fn_has_right_sig",
        r"  
            implementation
            function A[T](t: T): T;
            begin
                t
            end;
            end
        ",
    );

    let span = Span::zero("test");

    let (_, a_func) = module.unit.func_defs().next().unwrap();
    let a_sig = a_func.decl.sig();

    let type_args = TypeArgList::new([INT32], span.clone());

    let a_int_sig = a_sig
        .apply_type_args(a_func.decl.name.type_params.as_ref().unwrap(), &type_args);

    let expect_sig = FunctionSig {
        type_params: Some(ast::TypeList::new([FunctionSigTypeParam { is_ty: Type::Any }], span.clone())),
        result_ty: INT32.into(),
        params: vec![FunctionSigParam {
            ty: INT32.into(),
            modifier: None,
        }],
    };

    assert_eq!(expect_sig, a_int_sig);
}

#[test]
fn specialized_fn_with_specialized_params_has_right_params() {
    let span = builtin_span();
    let module = module_from_src(
        "specialized_fn_with_specialized_params_has_right_params",
        r"
            implementation

            type A[AT] = record
                a: AT;
            end;

            function B[BT](a: A[BT]): A[BT];
            begin
                a
            end;
            
            end
        ",
    );

    let int_params = TypeArgList::new([INT32], span.clone());

    let a_class = module
        .unit
        .type_decl_items()
        .filter_map(|(_vis, d)| match d {
            ast::TypeDeclItem::Struct(class) => Some(class.clone()),
            _ => unreachable!(),
        })
        .next()
        .unwrap();

    let a_int = specialize_struct_def(&a_class, &int_params, &module.context)
        .map(|class| Type::record(class.name.clone()))
        .unwrap();

    let (_, b_func) = module.unit.func_defs().next().unwrap();
    let b_sig = b_func.decl.sig();

    let b_int_sig = b_sig.apply_type_args(b_func.decl.name.type_params.as_ref().unwrap(), &int_params);

    let expect_sig = FunctionSig {
        type_params: Some(ast::TypeList::new([FunctionSigTypeParam { is_ty: Type::Any }], span.clone())),
        result_ty: a_int.clone(),
        params: vec![FunctionSigParam {
            ty: a_int,
            modifier: None,
        }],
    };

    assert_eq!(expect_sig, b_int_sig);
}

fn get_stmt(module_unit: &ModuleUnit, init_pos: usize) -> &Stmt {
    module_unit
        .unit
        .init
        .as_ref()
        .and_then(|block| block.body.get(init_pos))
        .unwrap_or_else(|| {
            panic!("expected unit to have a statement at position {} in its init block", init_pos)
        })
}

fn get_func_call(module_unit: &ModuleUnit, init_pos: usize) -> &Call {
    get_stmt(module_unit, init_pos)
        .as_call()
        .and_then(Call::as_func_call)
        .unwrap_or_else(|| {
            panic!("expected unit to have a function call at position {} in its init block", init_pos)
        })
}

#[test]
fn can_infer_ty_arg_from_real_record_arg() {
    const UNIT_NAME: &str = "can_infer_ty_args_from_real_record_arg";
    let module = module_from_src(
        UNIT_NAME,
        r"
            implementation
            uses System;
        
            type R = record
                member: Int32;
            end;
            
            function Func[T](item: T);
            begin
            end;
            
            initialization
                Func(R());
            end
        "
    );

    let test_span = Span::zero(UNIT_NAME);

    let (_, r_ty) = module.context
        .find_type(&IdentPath::from_parts(vec![
            Ident::new(UNIT_NAME, test_span.clone()),
            Ident::new("R", test_span.clone()),
        ])
        ).unwrap();

    let func_call = get_func_call(&module, 0);

    assert!(func_call.type_args.is_some());
    let type_args = func_call.type_args.as_ref().unwrap();

    assert_eq!(&type_args[0], r_ty);
}

#[test]
fn can_infer_ty_arg_from_real_int_arg() {
    let module = module_from_src(
        "can_infer_ty_args_from_real_args",
        r"
            implementation
            
            function Func[T](item: T);
            begin
            end;
            
            initialization
                Func(123);
            end
        "
    );

    let func_call = get_func_call(&module, 0);
    
    assert!(func_call.type_args.is_some());
    let type_args = func_call.type_args.as_ref().unwrap();
    
    assert_eq!(type_args[0], Type::Primitive(Primitive::Int32));
}

#[test]
fn can_infer_ty_arg_from_lambda() {
    let module = module_from_src(
        "can_infer_ty_arg_from_lambda",
        r"
            implementation
            
            function Func[T](item: function: T);
            begin
            end;
            
            initialization
                var f := lambda: 1;
                Func(f);
            end
        "
    );

    let func_call = get_func_call(&module, 1);
    assert!(func_call.type_args.is_some());
    let type_args = func_call.type_args.as_ref().unwrap();

    assert_eq!(type_args[0], Type::Primitive(Primitive::Int32));
}

#[test]
fn can_infer_func_ty_from_lambda_with_generic_return() {
    let module = module_from_src(
        "Test",
        r"   
            implementation
            uses System;

            type MyBox[T] = class
                val: T;
            end;

            initialization
                var f := lambda: begin
                    var box: MyBox[Integer] := MyBox(val: 123);
                    box
                end;
            end
        "
    );
    
    match get_stmt(&module, 0) {
        ast::Stmt::LocalBinding(binding) => {
            let f_val = binding.val
                .as_ref()
                .expect("expected f to have a value");
            let f_type = f_val
                .annotation()
                .ty()
                .as_func()
                .cloned()
                .expect("expected f to have a function type");
            
            assert_eq!("Test.MyBox[System.Int32]", f_type.result_ty.to_string());
        }
        
        other => panic!("expected binding, got: {}", other),
    }
}

#[test]
fn can_infer_from_enclosing_ty_param_in_method() {
    let module = module_from_src(
        "Test",
        r"   
            implementation
            uses System;

            type Wrapper[T] = class
                vals: array of T;
                function A;
            end;
            
            function Wrapper[T].A;
            begin
                // infer type of pointer here
                self.vals := [];
            end;

            end.
        ",
    );

    let (_, func_def) = module.unit.func_defs().next().unwrap();

    match &func_def.body.stmts[0] {
        ast::Stmt::Assignment(assignment) => {
            assert_eq!("self.vals", assignment.lhs.to_string());
            assert_eq!("array of T", assignment.rhs.annotation().ty().to_string());
        }
        
        other => panic!("expected assignment, got {other:?}")
    }
}


#[test]
fn can_infer_from_enclosing_ty_param_in_class_method() {
    let module = module_from_src(
        "Test",
        r"   
            implementation
            uses System;

            type Wrapper[T] = class
                class function A;
            end;
            
            class function Wrapper[T].A;
            begin
                // infer type of pointer here
                var vals: array of T := [];
            end;

            end.
        ",
    );

    let (_, func_def) = module.unit.func_defs().next().unwrap();

    match &func_def.body.stmts[0] {
        ast::Stmt::LocalBinding(binding) => {
            assert_eq!("vals", binding.name.to_string());
            assert_eq!("array of T", binding.val
                .as_ref()
                .expect("expected binding to have a value")
                .annotation()
                .ty()
                .to_string());
        }

        other => panic!("expected assignment, got {other:?}")
    }
}

// repro for a bug where the generic params were being resolved recursively during each step
// of the already-recursive sig type visit-and-resolve process
#[test]
fn apply_ty_args_to_sig_with_array_of_param_ty_produces_correct_ty() {
    let mut generic_ctx = GenericContext::empty();
    generic_ctx.add(
        TypeParam::new(builtin_ident("T")), 
        Type::generic_param(builtin_ident("T")).dyn_array()
    );

    let ty = Type::generic_param(builtin_ident("T"));
    let result = generic_ctx.apply_to_type(ty);  
    assert_eq!("array of T", result.to_string());

    let class_path = IdentPath::from_parts([builtin_ident("Box")]);
    let ty_params = TypeParamList::new([
        TypeParam::new(builtin_ident("T"))
    ], builtin_span());

    let sym = Symbol::from(class_path)
        .with_ty_params(Some(ty_params.clone()))
        .with_ty_args(Some(ty_params.into_type_args()));

    let sym_result = sym.clone().apply_type_args(&generic_ctx, &generic_ctx);
    assert_eq!("Box[array of T]", sym_result.to_string());
    
    let class = Type::class(sym);
    let class_result = generic_ctx.apply_to_type(class.clone());

    assert_eq!("Box[array of T]", class_result.to_string());
    
    let sig = FunctionSig {
        result_ty: class,  
        type_params: None,
        params: vec![],
    };
    let sig_result = generic_ctx.apply_to_sig(&sig);    
    assert_eq!("Box[array of T]", sig_result.result_ty.to_string());
}
