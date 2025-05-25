implementation
uses System;

type Greeting = interface
    function Greet();
end;

type HelloWorld = class of Greeting
    function Greet();
end;

function HelloWorld.Greet();
begin
    WriteLn('Hello, world');
end;

initialization
    var hw := HelloWorld();
    Greeting.Greet(hw);
end
