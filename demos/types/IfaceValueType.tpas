implementation
uses System;

type Greetable = interface
    function Name: String;
end;

type Person = record of Greetable
    name: String;
    
    function Name: String;
end;

function Person.Name(): String;
begin
    self.name
end;

initialization
    var alice := Person(name: 'Alice');
    WriteLn('hello, ' + alice.Name());
end
