implementation
uses System;

function IsFive(num: Integer): Boolean;
begin
    if num = 5 then begin
        exit true;
    end;

    WriteLn('it''s not five');
    false
end;

initialization
    for var i := 1 to 5 do begin
        if IsFive(i) then WriteLn('found five!');
    end;
end
