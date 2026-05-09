use crate::c::Class;
use crate::c::FieldName;
use crate::c::StructDef;
use crate::c::StructMember;
use crate::c::Type;
use crate::c::TypeDef;
use crate::c::Unit;
use crate::c::FunctionDef;
use crate::c::FunctionDecl;
use crate::c::Expr;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::Statement;
use crate::c::TypeDefName;
use crate::ir;

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
    pub fn get_box_type(&mut self, element_type: &ir::Type) -> BoxTypeID {
        if let Some(existing_id) = self.box_types_by_element.get(element_type) {
            return *existing_id;
        };

        let index = self.box_types_by_element.len();
        let id = BoxTypeID(index);

        let class = Class::gen_box_class(self.metadata, id, element_type.clone());
        self.classes.push(class);

        let element_field_ty = Type::from_metadata(element_type, self);

        let struct_name = TypeDefName::Box(id);
        let struct_def = StructDef::new(struct_name, false)
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

        self.type_defs.insert(struct_def.decl.name, TypeDef::Struct(struct_def));
        self.type_defs_order.insert(struct_name);

        let c_element_type = Type::from_metadata(element_type, self);
        for element_dep in c_element_type.type_def_deps() {
            self.type_defs_order.add_dependency(element_dep, struct_name);
        }

        self.box_types_by_element.insert(element_type.clone(), id);

        self.gen_box_element_method(id, element_type);

        id
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
