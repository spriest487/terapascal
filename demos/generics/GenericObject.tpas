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
    // first ctor needs explicit types, subsequently can be inferred from type of var 
    var box := TestBox with [array[1] of Integer](val: [5]);
    
    box := TestBox[array[1] of Integer](val: [3]);
    
    box := TestBox(val: [1]);

    var val := UnwrapTestBox(box);
    WriteLn(IntToStr(val[0]));
end
