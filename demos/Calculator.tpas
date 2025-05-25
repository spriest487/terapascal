unit Calculator;
implementation

const addCommand: Byte = 43; // '+'
    subCommand: Byte = 45; // '-'
    mulCommand: Byte = 42; // '*'
    divCommand: Byte = 47; // '/'

initialization
    WriteLn('Enter an expression e.g. ''2*3'', ''4/2'' or ''exit''.');
    
    while true do 
    begin
        var command := ReadLn.Trim;

        if command.Compare('exit') = 0 then 
            break;
    
        if command.Length = 0 then
            continue;
    
        var opIndex := -1;
        for var i := 0 to command.Length - 1 do begin
            var c := command.CharAt(i);
    
            if c = mulCommand or c = divCommand or c = addCommand or c = subCommand then 
            begin
                opIndex := i;
                break;
            end;
        end;
    
        if opIndex = -1 then 
        begin
            WriteLn('invalid command: ''' + command + '''');
            continue;
        end;
    
        var op := command.CharAt(opIndex);
        var lhs := command.SubString(0, opIndex)
            .Trim
            .StrToInt;
        var rhs := command.SubString(opIndex + 1, command.Length() - (opIndex + 1))
            .Trim
            .StrToInt;
    
        var result := case op of
            addCommand: lhs + rhs;
            subCommand: lhs - rhs;
            divCommand: lhs div rhs;
            mulCommand: lhs * rhs;
            else begin
                WriteLn('invalid operator found for command ' + command);
                -1
            end;
        end;
    
        WriteLn(' => ' + result);
    end;
end.
