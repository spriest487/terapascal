unit GenericInterface;

implementation

type
    ISomething[T] = interface
        function Value: T;
    end;

    SomethingClass = class of ISomething[Integer]
    public
        function Value: Integer;
    end;
    
    SomethingRecord = record of ISomething[String]
    public
        value: Integer;
    
        function Value: String;
    end;

function SomethingClass.Value: Integer;
begin
    123
end;

function SomethingRecord.Value: String;
begin
    self.value.ToString
end;

initialization
    var classInst: ISomething[Integer] := SomethingClass();
    WriteLn(classInst.Value.ToString);
    
    var recordInst := SomethingRecord(value: 456);
    WriteLn(recordInst.Value);
end.
