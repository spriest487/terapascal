use crate::ast::TypeDefName;
use crate::ast::TypeDef;
use crate::ast::Type;
use crate::ast::StructMember;
use crate::ast::StructDef;
use crate::ast::FieldName;
use crate::ast::Class;
use crate::ast::Unit;
use crate::ir;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BoxTypeID(pub usize);

impl<'a> Unit<'a> {
    pub fn get_box_type(&mut self, element_type: &ir::Type) -> BoxTypeID {
        if let Some(existing_id) = self.box_types_by_element.get(element_type) {
            return *existing_id;
        };

        let index = self.box_types_by_element.len();
        let id = BoxTypeID(index);

        let class = Class::gen_box_class(id, element_type.clone());
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

        id
    }
}
