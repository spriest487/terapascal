implementation

function A(name: String);
begin
    WriteLn('hello, ' + name);
end;

function B(name: String);
begin
    WriteLn('goodbye, ' + name);
end;

function Greet(greeting: function(String); name: String);
begin
    greeting(name);
end;

initialization
    Greet(A, 'world');
    Greet(B, 'everyone');
end
