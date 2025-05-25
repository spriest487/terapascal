use ir_lang::FunctionID;
use ir_lang::StringID;

#[derive(Debug, Clone)]
pub struct RuntimeFuncInfo {
    pub id: FunctionID,
    pub name: StringID,
}
