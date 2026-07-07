use std::borrow::Cow;
use std::fmt;
use std::sync::Arc;
use serde::Deserialize;
use serde::Serialize;
use crate::DeclPath;
use crate::IRFormatter;
use crate::MethodID;
use crate::RawFormatter;
use crate::Type;
use crate::TypeParam;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum FunctionIdentity {
    // function has a global path
    Global(DeclPath),

    Method {
        declaring_type: Type,
        id: MethodID,
        name: Arc<String>,
        type_params: Vec<TypeParam>,
    },

    // user-defined destructor method associated with a type
    Destructor {
        declaring_type: Type,
        id: MethodID,
        name: Arc<String>,
    },

    // function is anonymous (e.g. generated functions) but has a debug name
    Internal {
        name: Arc<String>,
        type_params: Vec<TypeParam>,
    },
}

impl FunctionIdentity {
    pub fn internal(
        name: impl Into<Arc<String>>,
        type_params: impl IntoIterator<Item=TypeParam>,
    ) -> Self {
        Self::Internal {
            name: name.into(),
            type_params: type_params.into_iter().collect(),
        }
    }

    pub fn declaring_type(&self) -> Option<&Type> {
        match self {
            FunctionIdentity::Global { .. }
            | FunctionIdentity::Internal { .. } => None,

            FunctionIdentity::Method { declaring_type, .. }
            | FunctionIdentity::Destructor { declaring_type, .. } => Some(declaring_type),
        }
    }

    pub fn type_params(&self) -> &[TypeParam] {
        match self {
            FunctionIdentity::Global(func_name) => &func_name.type_params,
            FunctionIdentity::Method { type_params, .. } => type_params,
            FunctionIdentity::Internal { type_params, .. } => type_params,

            FunctionIdentity::Destructor { .. } => &[],
        }
    }

    pub fn to_pretty_string(&'_ self, formatter: &impl IRFormatter) -> Cow<'_, String> {
        match self {
            FunctionIdentity::Global(path) => {
                Cow::Owned(path.to_string())
            },

            FunctionIdentity::Destructor { name, id: _, declaring_type } => {
                Cow::Owned(format!("{}.{name}", declaring_type.to_pretty_string(formatter)))
            }

            FunctionIdentity::Method { declaring_type, id: _, name, type_params } => {
                let mut result = declaring_type.to_pretty_string(formatter);
                result.push('.');
                result.push_str(name);
                if !type_params.is_empty() {
                    result.push('[');
                    for (i, param) in type_params.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(param.name.as_str());
                    }
                    result.push(']');
                }

                Cow::Owned(result)
            }

            FunctionIdentity::Internal { name, .. } => {
                Cow::Borrowed(name.as_ref())
            }
        }
    }

    pub fn global_name(&self) -> Option<&DeclPath> {
        match self {
            FunctionIdentity::Global(path) => Some(path),
            _ => None,
        }
    }
}

impl fmt::Display for FunctionIdentity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty_string(&RawFormatter))
    }
}