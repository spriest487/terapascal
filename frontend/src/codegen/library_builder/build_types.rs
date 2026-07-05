use crate::codegen::library_builder::LibraryBuilder;
use crate::typ;
use crate::ir;

impl<'a> LibraryBuilder<'a> {
    pub fn translate_type_param(&mut self, param: &typ::TypeParam) -> ir::TypeParam {
        let constraint = match &param.constraint {
            None => None,
            Some(is_type) => {
                let constraint_type = self.translate_type(&is_type.is_ty);
                Some(constraint_type)
            }
        };

        ir::TypeParam {
            name: param.name.name.clone(),
            constraint,
        }
    }

    pub fn translate_type_param_list(
        &mut self,
        param_list: Option<&typ::TypeParamList>,
    ) -> Vec<ir::TypeParam> {
        let Some(param_list) = param_list else {
            return Vec::new();
        };

        let mut params = Vec::with_capacity(param_list.len());
        for param in param_list.iter() {
            params.push(self.translate_type_param(param));
        }

        params
    }
}