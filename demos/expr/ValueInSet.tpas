implementation

type
    Numbers = set of 1 as Integer..10;

initialization
    var evens: Numbers := [1, 2, 6, 8, 10];
    evens := evens & ~[1];
    evens := evens | [4];
    
    unsafe begin
        var words := @evens as ^UInt64;
        WriteLn('word 1 is ' + words[0]);
        WriteLn('word 2 is ' + words[1]);
        WriteLn('word 3 is ' + words[2]);
        WriteLn('word 4 is ' + words[3]);
    end;

    for var i := 1 to 10 do begin
        var inSet := i in evens;

        WriteLn('is ' + i + ' even? ' + inSet);
    end;
end.
