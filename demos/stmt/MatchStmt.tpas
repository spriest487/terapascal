implementation
uses System;

type Things = variant
    FirstThing: Integer;
    SecondThing: Boolean;
end;

function WriteThing(thing: Things);
begin
    match thing of
        Things.FirstThing i: WriteLn(i.ToString());
        Things.SecondThing b: WriteLn(b.ToString());
    end;
end;

initialization
    var thing1 := Things.FirstThing(123);
    var thing2 := Things.SecondThing(true);
    
    WriteThing(thing1);
    WriteThing(thing2);
end
