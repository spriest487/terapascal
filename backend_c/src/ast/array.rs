use crate::ast::Class;
use crate::ast::Expr;
use crate::ast::FieldName;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::FunctionName;
use crate::ast::GlobalName;
use crate::ast::Statement;
use crate::ast::StructDef;
use crate::ast::StructMember;
use crate::ast::Type;
use crate::ast::TypeDef;
use crate::ast::TypeDefName;
use crate::ast::Unit;
use crate::ast::VariableID;
use crate::ast::BuiltinName;
use crate::ast::InfixOp;
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
    pub fn get_dyn_array_type(&mut self, element_type: &ir::Type) -> DynArrayTypeID {
        if let Some(existing_id) = self.dyn_array_types_by_element.get(element_type) {
            return *existing_id;
        };

        let index = self.dyn_array_types_by_element.len();
        let id = DynArrayTypeID(index);

        self.gen_dyn_array_methods(id, element_type);

        let class = Class::gen_dyn_array_class(self.metadata, id, element_type.clone());

        self.classes.push(class);

        let elements_field_ty = Type::from_metadata(&element_type.clone().ptr(), self);

        let struct_name = TypeDefName::DynArray(id);
        let struct_def = StructDef::new(struct_name, false)
            .with_comment(format!("array of {}", self.pretty_type(element_type)))
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
        
        self.type_defs.insert(struct_def.decl.name, TypeDef::Struct(struct_def));
        
        self.type_defs_order.insert(struct_name);
        
        let c_element_type = Type::from_metadata(element_type, self); 
        for element_dep in c_element_type.type_def_deps() {
            self.type_defs_order.add_dependency(element_dep, struct_name);
        }
        
        self.dyn_array_types_by_element.insert(element_type.clone(), id);

        id
    }

    fn gen_dyn_array_methods(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        self.gen_dyn_array_element_method(array_id, element_type);
        self.gen_dyn_array_length_method(array_id, element_type);
        self.gen_dyn_array_alloc_method(array_id, element_type);
    }

    fn gen_dyn_array_length_method(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        let arr_arg = Expr::local_var(ir::LocalID(1));

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

        let arr_arg = Expr::local_var(ir::LocalID(1));
        let index_arg = Expr::local_var(ir::LocalID(2));

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
        let arr_arg = Expr::local_var(ir::LocalID(0));
        let len_arg = Expr::local_var(ir::LocalID(1));

        let array_ptr_ty = Type::DefinedType(TypeDefName::DynArray(array_id)).ptr();

        let arr_ptr_var = VariableID::named("arr_ptr");

        let get_mem = Expr::Function(FunctionName::Builtin(BuiltinName::GetMem));
        let free_mem = Expr::Function(FunctionName::Builtin(BuiltinName::FreeMem));
        let zero_mem = Expr::Function(FunctionName::Builtin(BuiltinName::ZeroMemory));
        let forget_mem = Expr::Function(FunctionName::Forget);

        let c_element_type = Type::from_metadata(element_type, self);
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
