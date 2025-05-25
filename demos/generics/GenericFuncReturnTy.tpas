implementation
uses System;

type TestBox[T] = class
    val: T;
end;

function BoxUp[T](val: T): TestBox[T];
begin
    TestBox(val: val)
end;

initialization
    var i := 1;
    var iBox: TestBox[Integer] := BoxUp(i);
    
    var i2: Integer := iBox.val;
    
    System.WriteLn(System.IntToStr(i2));
    
    var s1 := 'test';
    var sBox: TestBox[System.String] := BoxUp(s1);
    var s2 := sBox.val;
    
    System.WriteLn(s2);
end
