program LinkedLists;

uses
    System.Collections;

begin
    var x := LinkedList with [Integer].Create;

    WriteLn('len of x: ' + x.Length);

    x.Add(123);
    x.Add(456);
    
    WriteLn('len of x: ' + x.Length);

    if x.TryGet(0) is Option.Some n then
        WriteLn('x[0]: ' + n)
    else
        WriteLn('missing item 0! should be 123');

    if x.TryGet(1) is Option.Some n then
        WriteLn('x[1]: ' + n)
    else
        WriteLn('missing item 0! should be 456');
        
    WriteLn('enumerating contents:');

    for var num in x do 
    begin
        WriteLn(num.ToString);
    end;
end
