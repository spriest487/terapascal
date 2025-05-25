implementation
uses System;

type Greeting = record
    message: String;
    
    function GetMessage(): String;    
    function Print();
end;

function Greeting.GetMessage(): String;
begin
    exit self.message;
end;

function Greeting.Print();
begin
    WriteLn(self.GetMessage());
end;

initialization
    var greeting := Greeting(message: 'Hello, world!');
    greeting.Print();
end
