use crate::ir;

#[derive(Debug, Clone)]
pub struct RuntimeFuncInfo {
    pub id: ir::FunctionID,
    pub name: ir::StringID,
    
    pub invoker: Option<ir::FunctionID>,
}
