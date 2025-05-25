implementation
uses System;

type INumberSource = interface
    function GetNumber: Integer
end;

type NumberHolder = class of INumberSource
    number: Integer;
    
    function GetNumber(): Integer;
end;

type NumberFactory = class of INumberSource
    function GetNumber(): Integer;
end;

function NumberHolder.GetNumber(): Integer;
begin
    self.number
end;

function NumberFactory.GetNumber(): Integer;
begin
    456
end;

initialization
    var source: INumberSource := NumberHolder(number: 123);

    WriteLn(source.GetNumber().ToString());
    
    source := NumberFactory();
    WriteLn(source.GetNumber().ToString());
end
