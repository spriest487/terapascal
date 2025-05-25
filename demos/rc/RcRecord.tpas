implementation
uses System;

type IntBox = class
    val: Integer;
end;

type Wrapper = record
    box: IntBox;
end;

type BigWrapper = record
    wrapper1: Wrapper;
    wrapper2: Wrapper
end;

function DoNothing(wrapper: Wrapper): Wrapper;
begin
    wrapper
end;

initialization
    begin
        var wrapped := IntBox(val: 123);
        var wrapper := Wrapper(box: wrapped);
    
        DoNothing(wrapper);
    end;
    
    begin
        var wrapper := Wrapper(box: IntBox(val: 123));
        var wrapper2 := BigWrapper(wrapper1: wrapper; wrapper2: wrapper);
    end;
end
