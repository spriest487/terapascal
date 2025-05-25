implementation
uses System;

function RefHello(var s: String);
begin
    // ok: ref param must already be initialized
    WriteLn('RefHello says: ' + s);

    s := 'Hello, world!';
end;

function OutHello(out s: String);
begin
    s := 'Hello again!';
end;

initialization
    var x := 'Hi!';
    
    RefHello(x);
    WriteLn('message is now: ' + x);
    
    OutHello(x);
    WriteLn('message is now: ' + x);
    
    var y: String;
    OutHello(y);
    WriteLn('message is now: ' + y);
end
