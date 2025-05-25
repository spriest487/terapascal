implementation 
uses System;

function ReturnSome[T](x: T): Option[T];
begin
    Option.Some(x);
end;

initialization
    var someX := ReturnSome[Integer](123);
    
    if someX is Option.Some x then
        WriteLn('someX is ' + IntToStr(x))
    else
        WriteLn('error: x is not Some')
end
