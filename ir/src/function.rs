use crate::IRFormatter;
use crate::InstructionList;
use crate::Label;
use crate::NamePath;
use crate::RawFormatter;
use crate::Ref;
use crate::StringID;
use crate::TagInfo;
use crate::Type;
use crate::TypeDefID;
use crate::VariableID;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
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
    pub id: VariableID,
    pub func: FunctionID,
    pub init_func: FunctionID,

    pub closure_id: TypeDefID,
    pub func_ty_id: TypeDefID,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FunctionSig {
    pub result_type: Type,
    pub param_types: Vec<Type>,
}

impl FunctionSig {
    pub fn new(param_tys: impl IntoIterator<Item=Type>, return_ty: Type) -> Self {
        Self {
            result_type: return_ty,
            param_types: param_tys.into_iter().collect(),
        }
    }
    
    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut result = String::from("function");

        if !self.param_types.is_empty() {
            result.push('(');

            for (i, param_ty) in self.param_types.iter().enumerate() {
                if i > 0 {
                    result.push_str("; ");
                }
                
                formatter.format_type(param_ty, &mut result).unwrap();
            }

            result.push(')');
        }

        if self.result_type != Type::Nothing {
            result.push_str(": ");
            formatter.format_type(&self.result_type, &mut result).unwrap();
        }
        
        result
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty_string(&RawFormatter))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub sig: FunctionSig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum FunctionIdentity {
    // function has a global path
    Path(NamePath),

    Method {
        declaring_type: Type,
        name: String,
        type_args: Vec<Type>,
    },

    // function is anonymous (e.g. generated functions) but has a debug name
    Internal(Rc<String>),
}

impl FunctionIdentity {
    pub fn internal(name: impl Into<String>) -> Self {
        Self::Internal(Rc::new(name.into()))
    }

    pub fn to_pretty_string(&'_ self, formatter: &impl IRFormatter) -> Cow<'_, String> {
        match self {
            FunctionIdentity::Path(path) => {
                Cow::Owned(path.to_pretty_string(formatter))
            },

            FunctionIdentity::Method { declaring_type, name, type_args } => {
                let mut result = declaring_type.to_pretty_string(formatter);
                result.push('.');
                result.push_str(name);
                if !type_args.is_empty() {
                    result.push('[');
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&arg.to_pretty_string(formatter));
                    }
                    result.push(']');
                }

                Cow::Owned(result)
            }

            FunctionIdentity::Internal(name) => {
                Cow::Borrowed(name.as_ref())
            }
        }
    }

    pub fn as_path(&self) -> Option<&NamePath> {
        match self {
            FunctionIdentity::Path(path) => Some(path),
            _ => None,
        }
    }
}

impl fmt::Display for FunctionIdentity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty_string(&RawFormatter))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionInfo {
    pub identity: FunctionIdentity,

    pub runtime_name: Option<StringID>,
    pub sig: FunctionSig,

    pub invoker: Option<FunctionID>,

    pub tags: Vec<TagInfo>,
}

impl FunctionInfo {
    pub fn tags_of_class(&self, id: TypeDefID) -> impl Iterator<Item=&TagInfo> {
        self.tags.iter().filter(move |t| t.class_id == id)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FunctionDef {
    pub debug_name: Option<String>,

    pub type_params: Vec<Arc<String>>,

    pub body: InstructionList,

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
        type_params: Vec<Arc<String>>,
        sig: FunctionSig,
        body: InstructionList,
    ) -> Self {
        Function::Local(FunctionDef {
            debug_name,
            type_params,
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
