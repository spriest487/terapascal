implementation

function GetAdder(a: Integer): function(Integer): Integer;
begin
    var func := function(b: Integer): Integer;
    begin
        a + b
    end;

    // the capture has already happened, so this shouldn't affect the result
    a += 1;
    
    func
end;

initialization
    var add4 := GetAdder(4);
    var z := add4(3);
    
    WriteLn(z.ToString());
end.
