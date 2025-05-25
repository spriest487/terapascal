implementation
uses System;

type MaybeIntBox = variant
    Some: Box[Integer];
    None;
end;

type Outer = class
    maybeBox: MaybeIntBox;
end;

initialization
    var maybeBox := MaybeIntBox.Some(
        NewBox[Integer](123)
    );
    
    var outer := Outer(
        maybeBox: maybeBox;
    );
    
    {
    if outer.maybeBox is MaybeIntBox.Some box then begin
    end;
    }
end
