{ closure which captures nothing should create a static closure rather than allocating one instance per call }
implementation

initialization
    var addToX := function(x: Integer): Integer;
    begin
        x + 123
    end;
    
    var bigAddToX := function(x: Integer): Integer;
    begin
        x + 999
    end;
    
    var add := addToX;
    var y := add(2);
    
    WriteLn(IntToStr(y));
    
    add := bigAddToX;
    y := add(2);

    WriteLn(IntToStr(y));
end
