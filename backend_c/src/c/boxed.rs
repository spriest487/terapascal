use crate::c::Class;
use crate::c::Expr;
use crate::c::FieldName;
use crate::c::FunctionDecl;
use crate::c::FunctionDef;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::Statement;
use crate::c::StructDef;
use crate::c::StructMember;
use crate::c::Type;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::ir;
use crate::c::type_map::TypeID;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BoxTypeID(pub usize);

impl BoxTypeID {
    pub fn class_ptr(&self) -> Expr {
        Expr::Global(GlobalName::BoxClassInstance(*self)).addr_of()
    }

    pub fn ptr_type(&self) -> Type {
        Type::Pointer(Box::new(Type::DefinedType(TypeDefName::Box(*self))))
    }
}

impl<'a> Unit<'a> {
    pub fn get_box_type(&mut self, element_type: &ir::Type) -> (TypeID, BoxTypeID) {
        let element_c_type = self.translate_type(element_type);
        let element_type_id = self.get_type_id(element_type);

        if let Some(existing_id) = self.box_types_by_element.get(&element_type_id) {
            let type_id = self.get_type_id(&element_type.boxed());
            return (type_id, *existing_id);
        };

        let box_index = self.box_types_by_element.len();
        let box_id = BoxTypeID(box_index);

        let box_def_name = TypeDefName::Box(box_id);
        let box_c_type = Type::DefinedType(box_def_name).ptr();
        let box_type_id = self.register_type(element_type.boxed(), box_c_type);

        let class = Class::gen_box_class(self, box_id, element_type.clone());
        self.classes.push(class);

        let element_field_ty = self.translate_type(element_type);

        let struct_def = StructDef::new(box_def_name, false)
            .with_comment(format!("box of {}", self.pretty_type(element_type)))
            .with_member(StructMember {
                name: FieldName::Rc,
                ty: Type::Rc,
                comment: None,
            })
            .with_member(StructMember {
                name: FieldName::BoxValue,
                ty: element_field_ty,
                comment: None,
            });

        self.register_type_def(box_type_id, struct_def.decl.name, struct_def, element_c_type.type_def_deps());

        self.box_types_by_element.insert(element_type_id, box_id);

        self.gen_box_element_method(box_id, element_type);

        (box_type_id, box_id)
    }

    fn gen_box_element_method(&mut self, box_id: BoxTypeID, element_type: &ir::Type) {
        let box_ptr_ty = Type::DefinedType(TypeDefName::Box(box_id)).ptr();

        let object_ptr_arg = Expr::arg_var(ir::ArgID(0));

        let element_func_body = vec![
            Statement::ReturnValue(
                object_ptr_arg
                    .cast(box_ptr_ty)
                    .arrow(FieldName::BoxValue)
                    .addr_of()
            ),
        ];

        self.functions.push(FunctionDef {
            decl: FunctionDecl {
                name: FunctionName::BoxValue(box_id),
                comment: Some(format!(
                    "generated element function for box of {}",
                    self.pretty_type(element_type),
                )),
                params: vec![Type::Rc.ptr()],
                return_ty: Type::Void.ptr(),
            },
            body: element_func_body,
        });
    }
}
