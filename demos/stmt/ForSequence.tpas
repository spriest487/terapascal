implementation

initialization
    // static array
    var numbers := [1, 2, 3];
    for var number in numbers do
        WriteLn(number.ToString);
    
    // dynarray    
    var letters: array of String := ['A', 'B', 'C'];
    for var letter in letters do
        WriteLn(letter);
    
    // string
    for var c in 'Hello, world' do
    begin
        // can't take the address of an iteration var
        var mutableChar := c;
        Write(StringFromBytes(@mutableChar, 1));
    end;
    WriteLn('!');
end
