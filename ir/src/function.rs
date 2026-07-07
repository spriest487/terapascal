mod function_identity;
mod function_sig;

pub use self::function_identity::*;
pub use self::function_sig::*;

use crate::ClosureIdentity;
use crate::IRFormatter;
use crate::InstructionList;
use crate::Label;
use crate::Ref;
use crate::StringID;
use crate::TagInfo;
use crate::Type;
use crate::TypeDefID;
use crate::VariableID;
use crate::Visibility;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

pub const BUILTIN_SRC: &str = "rt";

pub const RESULT_REF: Ref = Ref::Result;
pub const EXIT_LABEL: Label = Label(0);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct FunctionID(pub usize);

impl fmt::Display for FunctionID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function {}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StaticClosure {
    pub identity: ClosureIdentity,
    pub init_func: FunctionID,

    // ID of the global variable storing the singleton reference
    pub id: VariableID,

    // ID of the class implementing the closure
    pub closure_id: TypeDefID,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalFunctionRef {
    pub symbol: Arc<String>,
    pub src: Arc<String>,

    pub sig: Rc<FunctionSig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionParamInfo {
    pub name: Option<Arc<String>>,

    pub param_type: Type,

    pub tags: Vec<TagInfo>,
}

impl FunctionParamInfo {
    pub fn new(param_type: Type) -> Self {
        Self {
            name: None,
            param_type,
            tags: Vec::new(),
        }
    }

    pub fn with_name(mut self, name: impl Into<Arc<String>>) -> Self {
        self.name = Some(name.into());
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionInfo {
    pub identity: FunctionIdentity,

    // a private free function hints that the function should not be used by code from
    // outside its declaring library.
    // a private method function hints that the function should not be used by code outside
    // the family of the type it belongs to.
    pub visibility: Visibility,

    pub params: Vec<FunctionParamInfo>,
    pub result_type: Type,

    // RTTI fields
    pub runtime_name: Option<StringID>,
    pub invoker: Option<FunctionID>,

    pub tags: Vec<TagInfo>,
}

impl FunctionInfo {
    pub fn tags_of_class(&self, id: TypeDefID) -> impl Iterator<Item=&TagInfo> {
        self.tags.iter().filter(move |t| t.class_id == id)
    }

    pub fn sig(&self) -> FunctionSig {
        FunctionSig {
            result_type: self.result_type.clone(),
            param_types: self.params
                .iter()
                .map(|p| p.param_type.clone())
                .collect(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FunctionDef {
    pub body: InstructionList,

    pub sig: Rc<FunctionSig>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Function {
    External(ExternalFunctionRef),
    Local(FunctionDef),
}

impl Function {
    pub fn new_local_def(
        sig: impl Into<Rc<FunctionSig>>,
        body: InstructionList,
    ) -> Self {
        Function::Local(FunctionDef {
            sig: sig.into(),
            body,
        })
    }

    pub fn sig(&self) -> &Rc<FunctionSig> {
        match self {
            Function::External(external_func) => &external_func.sig,
            Function::Local(local_func) => &local_func.sig,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FunctionRef {
    pub def_id: FunctionID,
    pub args: Vec<Type>
}

impl FunctionRef {
    pub fn new(def_id: FunctionID) -> Self {
        Self {
            def_id,
            args: Vec::new(),
        }
    }

    pub fn with_args(self, args: impl IntoIterator<Item=Type>) -> Self {
        Self {
            def_id: self.def_id,
            args: args.into_iter().collect(),
        }
    }

    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut string = String::new();
        _ = formatter.format_func_ref(self, &mut string);
        string
    }
}