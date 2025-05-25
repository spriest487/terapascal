implementation
uses System;

type Something = record
    val: Integer;
end;

type MaybeSomething = variant
    Some: Something;
    None;
    Other: String;
end;

initialization
    var x := MaybeSomething.Some(Something(val: 1));
    if x is MaybeSomething.Some something then
        WriteLn('x is Some ' + IntToStr(something.val));
    
    if x is not MaybeSomething.None then
        WriteLn('x is not None');
    
    var y := MaybeSomething.None();
    if y is MaybeSomething.None then
        WriteLn('y is None');
    
    if y is not MaybeSomething.Some then
        WriteLn('y is not Some');
end
