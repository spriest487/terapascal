implementation
uses System;

function InferResultTy(i: Integer): Option[Integer];
begin
    if i > 1 then Option.Some(i) else Option.None()
end;

initialization
    var x := InferResultTy(2);
    
    WriteLn('x is: ' + if x is Option.Some xVal
        then xVal.ToString()
        else 'Nothing');
end
