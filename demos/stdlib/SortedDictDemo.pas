program SortedDictDemo;

uses
    System.SortedDict;
    
begin
    var dict := SortedDict with [Integer, String].Create;

    dict.Insert(100, 'One Hundred');
    dict.Insert(33, 'Thirty-Three');
    dict.Insert(50, 'Fifty');
    
    WriteLn('value of 100: ' + dict.Find(100).Get);
    WriteLn('value of 33: ' + dict.Find(33).Get);
    WriteLn('contains 44? ' + dict.Find(44).IsSome);
    
    WriteLn('removed 0? ' + dict.Remove(0).IsSome);
    WriteLn('removed value of 50: ' + dict.Remove(50).Get);
    
    for var entry in dict do 
    begin
        WriteLn(entry.key.ToString + ' = ' + entry.val);
    end;
end.
