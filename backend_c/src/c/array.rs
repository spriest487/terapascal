use crate::c::Class;
use crate::c::Expr;
use crate::c::FieldName;
use crate::c::FunctionDecl;
use crate::c::FunctionDef;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::InfixOp;
use crate::c::Statement;
use crate::c::Type;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::c::VariableID;
use crate::c::{BuiltinName, StructDef, StructMember};
use crate::c::type_map::TypeID;
use crate::ir;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ArrayTypeID(pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct DynArrayTypeID(pub usize);

impl DynArrayTypeID {
    pub fn class_ptr(&self) -> Expr {
        Expr::Global(GlobalName::DynArrayClassInstance(*self)).addr_of()
    }
}

impl<'a> Unit<'a> {
    pub fn translate_dyn_array_type(&mut self, element_type: &ir::Type) -> (TypeID, DynArrayTypeID) {
        let element_c_type = self.translate_type(element_type);
        let element_type_id = self.get_type_id(element_type);

        if let Some(existing_id) = self.dyn_array_types_by_element.get(&element_type_id) {
            let type_id = self.get_type_id(&element_type.dyn_array());
            return (type_id, *existing_id);
        };

        let index = self.dyn_array_types_by_element.len();
        let array_id = DynArrayTypeID(index);
        self.dyn_array_types_by_element.insert(element_type_id, array_id);

        let array_def_name = TypeDefName::DynArray(array_id);
        let array_c_type = Type::DefinedType(array_def_name).ptr();
        let array_type_id = self.register_type(element_type.dyn_array(), array_c_type);

        self.gen_dyn_array_methods(array_id, element_type);

        let class = Class::gen_dyn_array_class(self, array_id, element_type.clone());
        self.classes.push(class);

        let elements_field_ty = self.translate_type(&element_type.clone().ptr());

        let struct_def = StructDef::new(array_def_name, false)
            .with_comment(format!("array of {}", element_type.to_pretty_string(self.metadata)))
            .with_member(StructMember {
                name: FieldName::Rc,
                ty: Type::Rc,
                comment: None,
            })
            .with_member(StructMember {
                name: FieldName::DynArrayElements,
                ty: elements_field_ty,
                comment: None,
            })
            .with_member(StructMember {
                name: FieldName::DynArrayLength,
                ty: Type::Int32,
                comment: None,
            });

        let element_deps = element_c_type.type_def_deps();
        self.register_type_def(array_type_id, array_def_name, struct_def, element_deps);

        (array_type_id, array_id)
    }

    fn gen_dyn_array_methods(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        self.gen_dyn_array_element_method(array_id, element_type);
        self.gen_dyn_array_length_method(array_id, element_type);
        self.gen_dyn_array_alloc_method(array_id, element_type);
    }

    fn gen_dyn_array_length_method(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        let arr_arg = Expr::arg_var(ir::ArgID(0));

        let array_ptr_ty = Type::DefinedType(TypeDefName::DynArray(array_id)).ptr();

        self.functions.push(FunctionDef {
            decl: FunctionDecl {
                name: FunctionName::DynArrayLength(array_id),
                comment: Some(format!(
                    "generated length function for array of {}",
                    self.pretty_type(element_type),
                )),
                params: vec![Type::Rc.ptr()],
                return_ty: Type::Int32,
            },
            body: vec![
                Statement::ReturnValue(
                    arr_arg
                        .cast(array_ptr_ty)
                        .arrow(FieldName::DynArrayLength)
                ),
            ],
        });
    }

    fn gen_dyn_array_element_method(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        let array_ptr_ty = Type::DefinedType(TypeDefName::DynArray(array_id)).ptr();

        let arr_arg = Expr::arg_var(ir::ArgID(0));
        let index_arg = Expr::arg_var(ir::ArgID(1));

        let element_func_body = vec![
            Statement::Expr(Expr::Function(FunctionName::DynArrayBoundsCheck)
                .call([
                    arr_arg.clone(),
                    index_arg.clone(),
                ])),
            Statement::ReturnValue(
                arr_arg
                    .cast(array_ptr_ty)
                    .arrow(FieldName::DynArrayElements)
                    .index(index_arg.cast(Type::SizeType))
                    .addr_of()
            ),
        ];

        self.functions.push(FunctionDef {
            decl: FunctionDecl {
                name: FunctionName::DynArrayGetElement(array_id),
                comment: Some(format!(
                    "generated element function for array of {}",
                    self.pretty_type(element_type),
                )),
                params: vec![Type::Rc.ptr(), Type::Int32],
                return_ty: Type::Void.ptr(),
            },
            body: element_func_body,
        });
    }

    fn gen_dyn_array_alloc_method(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        let arr_arg = Expr::arg_var(ir::ArgID(0));
        let len_arg = Expr::arg_var(ir::ArgID(1));

        let array_ptr_ty = Type::DefinedType(TypeDefName::DynArray(array_id)).ptr();

        let arr_ptr_var = VariableID::named("arr_ptr");

        let get_mem = Expr::Function(FunctionName::Builtin(BuiltinName::GetMem));
        let free_mem = Expr::Function(FunctionName::Builtin(BuiltinName::FreeMem));
        let zero_mem = Expr::Function(FunctionName::Builtin(BuiltinName::ZeroMemory));
        let forget_mem = Expr::Function(FunctionName::Forget);

        let c_element_type = self.translate_type(element_type);
        let elements_ptr_type = c_element_type.clone().ptr();

        let alloc_size = Expr::SizeOf(c_element_type.clone())
            .infix_op(InfixOp::Mul, len_arg.clone().cast(Type::SizeType));
        
        let arr_elements_ptr = Expr::Variable(arr_ptr_var.clone())
            .arrow(FieldName::DynArrayElements);

        self.functions.push(FunctionDef {
            decl: FunctionDecl {
                name: FunctionName::DynArrayAlloc(array_id),
                comment: Some(format!(
                    "generated alloc function for array of {}",
                    self.pretty_type(element_type),
                )),
                params: vec![
                    Type::Rc.ptr(),
                    Type::Int32,
                ],
                return_ty: Type::Void,
            },
            body: vec![
                // declare array pointer and cast arg to it
                Statement::VariableDecl {
                    id: arr_ptr_var.clone(),
                    ty: array_ptr_ty.clone(),
                    null_init: false,
                },
                Statement::assign(
                    Expr::Variable(arr_ptr_var.clone()), 
                    arr_arg.cast(array_ptr_ty)
                ),

                // free the old memory
                Statement::Expr(free_mem.call([
                    Expr::Variable(arr_ptr_var.clone())
                        .arrow(FieldName::DynArrayElements)
                        .cast(Type::UChar.ptr()),
                ])),

                // alloc and zero the new memory
                Statement::assign(
                    arr_elements_ptr.clone(),
                    get_mem.call([alloc_size.clone()])
                        .cast(elements_ptr_type),
                ),
                Statement::Expr(zero_mem.call([
                    arr_elements_ptr.clone().cast(Type::UChar.ptr()),
                    alloc_size,
                ])),
                
                // if immortal (strong count < 0), forget the new memory
                Statement::if_then(
                    Expr::Variable(arr_ptr_var.clone())
                        .arrow(FieldName::Rc)
                        .field(FieldName::RcStrongCount)
                        .infix_op(InfixOp::Lt, Expr::LitInt(0)),
                    [
                        Statement::Expr(forget_mem.call([
                            arr_elements_ptr,
                            Expr::LitCString("forget".to_string()),
                        ])),
                    ]
                ),
                
                Statement::assign(
                    Expr::Variable(arr_ptr_var.clone()).arrow(FieldName::DynArrayLength),
                    len_arg,
                ),
            ],
        });
    }
}
