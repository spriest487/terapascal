implementation
uses System;

function A(): Boolean;
begin
    WriteLn('called A');
    false
end;

function B(): Boolean;
begin
    WriteLn('called B');
    true
end;

initialization
    WriteLn('part 1');
    if A() and B() then
        WriteLn('unreachable');
        
    WriteLn('part 2');
    if not A() and not B() then 
        WriteLn('unreachable');

    WriteLn('part 3');
    if B() and not A() then 
        WriteLn('here!');
end
