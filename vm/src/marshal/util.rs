use crate::marshal::unmarshal_bytes;
use crate::marshal::MarshalResult;
use crate::DynValue;

#[derive(Debug, Clone)]
pub struct UnmarshalledValue<T> {
    pub value: T,
    pub byte_count: usize,
}

impl From<UnmarshalledValue<DynValue>> for DynValue {
    fn from(val: UnmarshalledValue<DynValue>) -> Self {
        val.value
    }
}

impl<T> UnmarshalledValue<T> {
    pub fn map<U, F>(self, f: F) -> UnmarshalledValue<U>
    where
        F: FnOnce(T) -> U,
    {
        UnmarshalledValue {
            value: f(self.value),
            byte_count: self.byte_count,
        }
    }
}

pub fn unmarshal_from_ne_bytes<T, FUn, const COUNT: usize>(
    in_bytes: &[u8],
    f_un: FUn,
) -> MarshalResult<UnmarshalledValue<T>>
where
    FUn: Fn([u8; COUNT]) -> T,
{
    let bytes = unmarshal_bytes(in_bytes)?;
    let value = f_un(bytes);

    Ok(UnmarshalledValue {
        value,
        byte_count: bytes.len(),
    })
}
