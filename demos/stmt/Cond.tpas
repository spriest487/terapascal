implementation
uses System;

function Maybe(cond: Boolean): Integer;
begin
    if cond then 1 else 2
end;

initialization
    var x: Integer := if true then 123 else 312;
    WriteLn(IntToStr(x));
    
    var z := if false then 'first'
        else if false then 'second'
        else 'third';
    
    WriteLn(z);
    
    WriteLn(begin
        var a: Integer := 543;
        var b: Integer := 345;
        var cond: Boolean := false;
    
        IntToStr(if cond then a else b)
    end);
    
    WriteLn(IntToStr(Maybe(true)));
    WriteLn(IntToStr(Maybe(false)));
end
