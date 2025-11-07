use crate::ast::Class;
use crate::ast::Expr;
use crate::ast::FieldName;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::FunctionName;
use crate::ast::Statement;
use crate::ast::Type;
use crate::ast::TypeDefName;
use crate::ast::Unit;
use crate::ir;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ArrayTypeID(pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct DynArrayTypeID(pub usize);

impl Unit {
    pub fn get_dyn_array_type(&mut self, element_type: &ir::Type) -> DynArrayTypeID {
        if let Some(existing_id) = self.dyn_array_types_by_element.get(element_type) {
            return *existing_id;
        };

        let index = self.dyn_array_types_by_element.len();
        let id = DynArrayTypeID(index);
        
        self.gen_dyn_array_methods(id, element_type);

        let class = Class::gen_dyn_array_class(id, element_type.clone());

        self.classes.push(class);
        
        self.dyn_array_types_by_element.insert(element_type.clone(), id);

        id
    }

    fn gen_dyn_array_methods(&mut self, array_id: DynArrayTypeID, element_type: &ir::Type) {
        let array_ptr_ty = Type::DefinedType(TypeDefName::DynArray(array_id));

        let element_func_name = FunctionName::DynArrayGetElement(array_id);

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
                    .arrow(FieldName::ID(ir::DYNARRAY_PTR_FIELD))
                    .index(index_arg.cast(Type::SizeType))
                    .addr_of()
            ),
        ];
        self.functions.push(FunctionDef {
            decl: FunctionDecl {
                name: element_func_name,
                comment: Some(format!(
                    "generated dynarray element function for array of {}",
                    self.pretty_type(element_type),
                )),
                params: vec![Type::Rc.ptr(), Type::Int32],
                return_ty: Type::Void.ptr(),
            },
            body: element_func_body,
        });
    }
}
