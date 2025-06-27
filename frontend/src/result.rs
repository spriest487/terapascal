pub trait ErrorContinue: Sized {
    type Item;
    type Error;
    type ErrorSink<'a>;

    fn or_continue(
        self,
        errors: Self::ErrorSink<'_>,
        or_default: Self::Item,
    ) -> Self::Item {
        self.or_continue_with(errors, || or_default)
    }

    fn or_continue_with<DefaultFn>(
        self,
        errors: Self::ErrorSink<'_>,
        f: DefaultFn,
    ) -> Self::Item
    where
        DefaultFn: FnOnce() -> Self::Item;

    fn ok_or_continue<'a>(self, errors: Self::ErrorSink<'a>) -> Option<Self::Item>;
}
