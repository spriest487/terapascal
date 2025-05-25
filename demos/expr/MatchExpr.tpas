implementation
uses System;

type Things = variant
    FirstThing: Integer;
    SecondThing: Boolean;
    ThirdThing: String;
end;

function WriteThing(thing: Things);
begin
    WriteLn(match thing of
        Things.FirstThing i: i.ToString();
        Things.SecondThing b: b.ToString();
        else 'something else'
    end);
end;

initialization
    var thing1 := Things.FirstThing(123);
    var thing2 := Things.SecondThing(true);
    var thing3 := Things.ThirdThing('hello world');
    
    WriteThing(thing1);
    WriteThing(thing2);
    WriteThing(thing3);
end
