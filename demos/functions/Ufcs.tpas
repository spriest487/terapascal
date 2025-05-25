implementation
uses System;

type Person = record
    name: String;
end;

function Greet(person: Person);
begin
    WriteLn('hello, ' + person.name);
end;

initialization
    var world := Person(name: 'World');
    world.Greet();
end
