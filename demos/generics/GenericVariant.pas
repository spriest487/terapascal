implementation
uses System;

type
    MyEither[TA, TB] = variant
        First: TA;
        Second: TB;
    end;
    
function PrintEither(e: MyEither[String, Boolean]);
begin
    match e of
        MyEither.First a: 
            WriteLn(a);
        MyEither.Second b: 
            WriteLn(b.ToString);
        else
            raise 'unreachable';
    end;
end;

initialization
    var some: Option[String] := Option.Some('thing');
    
    var eitherFirst := MyEither with [String, Boolean].First('a string');
    var eitherSecond: MyEither[String, Boolean] := MyEither.Second(true);
    
    PrintEither(eitherFirst);
    PrintEither(eitherSecond);
    
    if some is Option.Some val then
        WriteLn('some is ' + val);
    
    var none: Option[String] := Option.None();
    
    if none is Option.Some val then
        WriteLn('oops, none had a value: ' + val)
    else if none is Option.None then
        WriteLn('none is empty');
end
