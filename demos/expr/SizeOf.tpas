implementation
uses System;

type SomeRecord = record
    f1: Integer;
    f2: Integer;
    f3: Integer;
end;

initialization
    var intSize := sizeof(Integer);
    WriteLn('int size: ' + intSize.ToString());
    
    var recordSize := sizeof(SomeRecord);
    WriteLn('SomeRecord size: ' + recordSize.ToString());
    
    var arrayOf20IntsSize := sizeof(array[20] of Integer);
    WriteLn('array of 20 ints size: ' + arrayOf20IntsSize.ToString());
    
    var strSize := sizeof(String);
    var ptrSize := sizeof(Pointer);
    WriteLn('string size = pointer size: ' + (strSize = ptrSize));
end
