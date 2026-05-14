use crate::c::FunctionName;
use crate::ir;

#[derive(Debug, Clone)]
pub struct RuntimeFuncInfo {
    pub id: ir::FunctionID,
    pub name: Option<ir::StringID>,

    pub invoker: Option<FunctionName>,
}
