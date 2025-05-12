implementation
uses System;

type TestBox[Val] = class
    val: Val;
end;

function UnwrapTestBox[T](box: TestBox[T]): T;
begin
    box.val
end;

initialization
    // first line needs explicit args, second can be inferred from type of var 
    var box := TestBox with [Integer](val: 1);
    box := TestBox(val: 1);

    var val := UnwrapTestBox[Integer](box);
    
    WriteLn(IntToStr(val));
end
