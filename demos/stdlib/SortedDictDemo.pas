program SortedDictDemo;

uses
    System.SortedDict;
    
begin
    var dict := SortedDict with [Integer, String].Create;

    dict.Insert(100, 'One Hundred');
    dict.Insert(33, 'Thirty-Three');
    dict.Insert(50, 'Fifty');
    
    WriteLn(dict.Find(100).Get);
    WriteLn(dict.Find(33).Get);
    WriteLn(dict.Find(44).IsSome.ToString);
    
    for var entry in dict do 
    begin
        WriteLn(entry.key.ToString + ' = ' + entry.val);
    end;
end.
