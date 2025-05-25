implementation
uses System;

type BigBox = class
    a: Box[Integer];
    b: Box[Integer];
end;

initialization
    var boxes := BigBox(a: NewBox[Integer](1); b: NewBox[Integer](2));
    
    var a := boxes.a;
    var aVal := Unbox[Integer](a);
    
    var bVal := Unbox[Integer](boxes.b);
    
    WriteLn('box contents:');
    WriteLn(IntToStr(aVal));
    WriteLn(IntToStr(bVal));
end
