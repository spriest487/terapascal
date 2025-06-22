use crate::DiagnosticMessage;
use crate::DiagnosticOutput;
use std::iter::once;
use std::mem;

#[derive(Debug)]
pub struct AggregateError<T, E> {
    pub item: Box<T>,

    pub first: Box<E>,
    pub rest: Vec<E>,
}

pub type AggregateResult<T, E> = Result<T, AggregateError<T, E>>;

impl<T, E> AggregateError<T, E> {
    pub fn new(item: impl Into<Box<T>>, first_err: impl Into<Box<E>>) -> Self {
        Self {
            item: item.into(),
            first: first_err.into(),
            rest: Vec::new(),
        }
    }

    pub fn unwrap(self) -> (T, Vec<E>) {
        let mut errors = self.rest;
        errors.insert(0, *self.first);

        (*self.item, errors)
    }
    
    pub fn and_continue(mut self, errors: &mut Vec<E>) -> T {
        errors.push(*self.first);
        errors.append(&mut self.rest);

        *self.item
    }
    
    pub fn map<F, Next>(result: AggregateResult<T, E>, f: F) -> AggregateResult<Next, E> 
    where
        F: Fn(T) -> AggregateResult<Next, E>
    {
        match result {
            Ok(item) => {
                f(item)
            },

            Err(mut err) => {
                match f(*err.item) {
                    Ok(next_item) => {
                        Err(AggregateError {
                            item: Box::new(next_item),
                            first: err.first,
                            rest: err.rest,
                        })
                    }
                    
                    Err(mut next_err) => {
                        // swap the two errors, so the first error comes first
                        mem::swap(&mut next_err.first, &mut err.first);
                        mem::swap(&mut next_err.rest, &mut err.rest);
                        
                        // and add the second error to the rest list 
                        next_err.rest.push(*err.first);
                        next_err.rest.append(&mut err.rest);
                        
                        Err(next_err)
                    }
                }
            }
        }
    }
    
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

    pub fn chain<F>(result: AggregateResult<T, E>, f: F) -> AggregateResult<T, E> 
    where
        F: FnOnce() -> Result<(), E>
    {
        match result {
            Err(mut err) => {
                match f() {
                    Ok(()) => {
                        Err(err)
                    },

                    Err(new_err) => {
                        err.rest.push(new_err);
                        
                        Err(err)
                    }
                }
            }
            
            Ok(item) => {
                match f() {
                    Ok(()) => {
                        Ok(item)
                    },
                    
                    Err(err) => {
                        Err(AggregateError::new(item, err))
                    }
                }
            }
        }
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
                let main = err.main();
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
        if self.rest.is_empty() {
            *self.first
        } else {
            E::from_aggregate_error(self)
        }
    }
}
