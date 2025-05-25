implementation
uses System;

type 
    B = class;
    A = class
        b: B;
    end;
    
    B = class
        value: Integer;
    end;

initialization
    var b := B(value: 123);
    var a := A(b: b);
    
    WriteLn(a.b.value.ToString());
end.
