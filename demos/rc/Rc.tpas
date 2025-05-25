implementation
uses System;

type MyBox = class
    val1: Integer;
    val2: Boolean;
end;

type BoxBox = class
    box: MyBox;
end;

function DoNothing(bb: BoxBox);
begin
end;

initialization
    var box1 := MyBox(val1: 1; val2: true);
    
    var boxbox := BoxBox(box: box1);
    DoNothing(boxbox);
end
