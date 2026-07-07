use crate::IRFormatter;
use crate::ObjectID;
use crate::RawFormatter;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;

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

    pub fn contains_generic_params(&self) -> bool {
        self.result_type.contains_generic_params()
            || self.param_types.iter().any(|ty| ty.contains_generic_params())
    }

    pub fn to_function_type(self: &Rc<Self>) -> Type {
        Type::Function(self.clone())
    }

    pub fn into_function_type(self) -> Type {
        Rc::new(self).to_function_type()
    }

    pub fn to_closure_id(self: &Rc<Self>) -> ObjectID {
        ObjectID::AnyClosure(self.clone())
    }

    pub fn into_closure_id(self) -> ObjectID {
        Rc::new(self).to_closure_id()
    }

    pub fn to_closure_ptr_type(self: &Rc<Self>) -> Type {
        self.to_closure_id().to_object_type()
    }

    pub fn into_closure_ptr_type(self) -> Type {
        self.into_closure_id().to_object_type()
    }

    /// Create the sig of the function pointer type that needs to be stored in a closure
    /// that calls a function with this sig. Inserts a new 0th parameter of the anonymous
    /// closure type.
    pub fn to_closure_function_ptr_sig(self: &Rc<Self>) -> Self {
        let mut sig = (**self).clone();
        sig.param_types.insert(0, self.to_closure_ptr_type());

        sig
    }

    pub fn into_closure_function_ptr_sig(self) -> Self {
        Rc::new(self).to_closure_function_ptr_sig()
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty_string(&RawFormatter))
    }
}