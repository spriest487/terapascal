implementation
uses System;

function GenFunc[T](t: T);
begin
    if t is String s then
        WriteLn('called GenFunc with a string')
    else
        WriteLn('called GenFunc with something else');
end;

initialization
    GenFunc(1);
    GenFunc('a');
    GenFunc(true);
end
