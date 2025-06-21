use crate::DiagnosticMessage;
use crate::DiagnosticOutput;
use crate::Severity;
use std::iter::once;

#[derive(Debug)]
pub struct AggregateError<T, E> {
    pub item: Box<T>,

    pub first: Box<E>,
    pub rest: Vec<E>,
}

impl<T, E> AggregateError<T, E> {
    pub fn result(item: T, mut errors: Vec<E>) -> Result<T, Self> {
        if errors.is_empty() {
            return Ok(item);
        }

        let first_err = errors.remove(0);

        Err(AggregateError {
            item: Box::new(item),
            first: Box::new(first_err),
            rest: errors,
        })
    }
}

impl<T, E: DiagnosticOutput> AggregateError<T, E> {
    pub fn first_title(&self) -> String {
        self.first.title()
    }

    pub fn first_label_text(&self) -> Option<String> {
        self.first.label().and_then(|label| label.text)
    }

    pub fn rest_messages(&self) -> Vec<DiagnosticMessage> {
        self.rest
            .iter()
            .flat_map(|err| {
                let main = err.main(Severity::Error);
                let see_also = err.see_also();

                once(main).chain(see_also)
            })
            .collect()
    }
}

pub trait FromAggregateError<T> : Sized {
    fn from_aggregate_error(err: AggregateError<T, Self>) -> Self;
}

impl<T, E: FromAggregateError<T>> AggregateError<T, E> {
    pub fn into_err(self) -> E {
        E::from_aggregate_error(self)
    }
}
