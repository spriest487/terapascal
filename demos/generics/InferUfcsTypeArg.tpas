implementation
uses System;

type TestBox[T] = class
    t: T;
end;

function TestUnwrap[T](b: TestBox[T]): T;
begin
    b.t
end;

initialization
    var x: TestBox[Integer] := TestBox(t: 123);
    var y := TestUnwrap(x);
end
