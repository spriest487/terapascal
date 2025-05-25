implementation

type 
    MyRecord = record
        val: Integer;
        
        function Add(i: Integer);
    end;
    
function MyRecord.Add(i: Integer);
begin
    self.val += i;
end;
    
initialization
    var x := MyRecord(val: 100);
    x.Add(23);
    
    WriteLn(x.val.ToString);
end
