implementation
uses System;

type
    SimpleRecord = record
        a: Integer;
        b: Single;
        c: Boolean;
    end;
    
    SomeClass = class
    end;
    
    RecordWithClass = record
        a: SomeClass;
    end;
    
    RecordArray = array[2] of SimpleRecord;

initialization
    // invalid: unable to infer type
    // var x := default;
    
    var x := default(Integer);
    WriteLn('default integer is ' + x);

    var p: Pointer := default;
    WriteLn('default ptr is ' + p);

    var simple := default(SimpleRecord);
    WriteLn('default SimpleRecord.a is ' + simple.a);
    WriteLn('default SimpleRecord.b is ' + simple.b);
    WriteLn('default SimpleRecord.c is ' + simple.c);
    
    unsafe begin
        var classInst := default(SomeClass);
        WriteLn('default SomeClass is ' + classInst as Pointer);

        var recordWithInst := default(RecordWithClass);
        WriteLn('default RecordWithClass.a is ' + recordWithInst.a as Pointer);
    end;
    
    var simples := default(RecordArray);
    WriteLn('default RecordArray[0].a is ' + simples[0].a);
    WriteLn('default RecordArray[1].a is ' + simples[1].a);
end.
