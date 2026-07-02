use crate::Type;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum MetadataError {
    #[error("Missing definition for type {0}")]
    MissingTypeDef(Type),
}

pub type MetadataResult<T> = Result<T, MetadataError>;