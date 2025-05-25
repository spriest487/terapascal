use crate::{Instruction, StringID};
use crate::InstructionFormatter;
use crate::Label;
use crate::LocalID;
use crate::NamePath;
use crate::RawInstructionFormatter;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;

pub const BUILTIN_SRC: &str = "rt";

pub const RETURN_LOCAL: LocalID = LocalID(0);
pub const RETURN_REF: Ref = Ref::Local(RETURN_LOCAL);
pub const EXIT_LABEL: Label = Label(0);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct FunctionID(pub usize);

impl fmt::Display for FunctionID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StaticClosureID(pub usize);

impl fmt::Display for StaticClosureID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "static closure #{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StaticClosure {
    pub id: StaticClosureID,
    pub init_func: FunctionID,

    pub closure_id: TypeDefID,
    pub func_ty_id: TypeDefID,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub param_tys: Vec<Type>,
}

impl FunctionSig {
    pub fn new(param_tys: impl IntoIterator<Item=Type>, return_ty: Type) -> Self {
        Self {
            return_ty,
            param_tys: param_tys.into_iter().collect(),
        }
    }
    
    pub fn to_pretty_string(&self, formatter: &impl InstructionFormatter) -> String {
        let mut result = String::from("function");

        if !self.param_tys.is_empty() {
            result.push_str("(");

            for (i, param_ty) in self.param_tys.iter().enumerate() {
                if i > 0 {
                    result.push_str("; ");
                }
                
                formatter.format_type(param_ty, &mut result).unwrap();
            }

            result.push_str(")");
        }

        if self.return_ty != Type::Nothing {
            result.push_str(": ");
            formatter.format_type(&self.return_ty, &mut result).unwrap();
        }
        
        result
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty_string(&RawInstructionFormatter))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub sig: FunctionSig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub runtime_name: Option<StringID>,
    
    pub global_name: Option<NamePath>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FunctionDef {
    pub debug_name: Option<String>,

    pub body: Vec<Instruction>,

    pub sig: FunctionSig,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Function {
    External(ExternalFunctionRef),
    Local(FunctionDef),
}

impl Function {
    pub fn new_local_def(
        debug_name: Option<String>,
        sig: FunctionSig,
        body: Vec<Instruction>
    ) -> Self {
        Function::Local(FunctionDef {
            debug_name,
            sig,
            body,
        })
    }
    
    pub fn debug_name(&self) -> Option<&String> {
        match self {
            Function::External(ExternalFunctionRef { symbol, .. }) => Some(symbol),
            Function::Local(FunctionDef { debug_name, .. }) => debug_name.as_ref(),
        }
    }

    pub fn sig(&self) -> &FunctionSig {
        match self {
            Function::External(external_func) => &external_func.sig,
            Function::Local(local_func) => &local_func.sig,
        }
    }
}
