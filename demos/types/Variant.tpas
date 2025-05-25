implementation
uses System;

type Numbers = variant
    One: Byte;
    Two: Integer;
    Three: String;
end;

initialization
    var n := Numbers.Three('three');
    
    if n is Numbers.Three three then
    begin
        WriteLn('Three is ' + three);
    end;
end
